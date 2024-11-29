import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale


def formatDate(date: LocalDate)(using locale: Locale): String = {
  val formatter = DateTimeFormatter.ofPattern("MMMM dd, yyyy", locale)
  date.format(formatter)
}

def printNumber(using number: Int): Unit = println(s"Number is $number")

trait Show[T]:
  def show(value: T): String

given Show[Int] with
  def show(value: Int): String = s"Int: $value"

given Show[String] with
  def show(value: String): String = s"String: $value"

// Automatically wrap existing Show[T] in Option[Show[T]]
given optionShow[T](using show: Show[T]): Option[Show[T]] = Some(show)

// Provide a fallback for missing Show[T]
given noShow[T]: Option[Show[T]] = None

def printShow[T](value: T)(using Show[T]): Unit =
  val showInstance = summon[Show[T]]
  println(showInstance.show(value))


given [T: Show]: Show[List[T]] with
  def show(value: List[T]): String = value.map(summon[Show[T]].show).mkString(", ")

def display[T: Show](value: T): String = summon[Show[T]].show(value)


def combineAndPrint[T](x: T, y: T)(using Show[T]): Unit =
  val showInstance = summon[Show[T]] // Explicitly retrieve the `Show` instance
  println(showInstance.show(x) + " & " + showInstance.show(y))

  
trait Greeting[T]:
  def greet(value: T): String


given Greeting[String] with
  def greet(value: String) = s"Hello, $value!"

given Greeting[Int] with
  def greet(value: Int) = s"The number is $value."

given Locale = Locale.US  

trait Database:
  def query(sql: String): String

given Database with
  def query(sql: String) = s"Executing query: $sql"

trait Logger:
  def log(message: String): Unit

given Logger with
  def log(message: String): Unit = println(s"[INFO]: $message")

trait Authenticator:
  def authenticate(user: String): Boolean

given Authenticator with
  def authenticate(user: String): Boolean = user == "admin"

def performTask(user: String)(using logger: Logger, auth: Authenticator): Unit = {
  if summon[Authenticator].authenticate(user) then
    summon[Logger].log(s"User '$user' authenticated.")
  else
    summon[Logger].log(s"User '$user' failed authentication.")
}

def fetchUser(id: Int)(using db: Database): String =
  db.query(s"SELECT * FROM users WHERE id = $id")

@main def learnsummon1 =
    case class Config(appName: String, version: String)
    given Config = Config("MyApp", "1.0.0")
    val config: Config = summon[Config]
    println(s"App Name: ${config.appName}, Version: ${config.version}")

    def add[T: Numeric](a: T, b: T): T = {
        val numeric = summon[Numeric[T]]
        numeric.plus(a, b)
    }

    println(add(5, 10))       // Works with Int
    println(add(5.5, 10.2))   // Works with Double
    def greet[T: Greeting](value: T): String = summon[Greeting[T]].greet(value)
    println(greet("Alice"))
    println(greet(42))

    val today = LocalDate.now()
    println(formatDate(today)) // Uses the given Locale (US)

    println(fetchUser(123)) // Summons the given Database instance  
    {
        given Int = 42
        printNumber // Uses default given
    }
    {
        given Int = 99
        printNumber // Uses the new given
    }
    performTask("admin")
    performTask("guest")
    println(display(123))
    println(display(List(1, 2, 3)))

@main def learnsummon2(): Unit =
  val greeting = summon[Greeting[String]] // Retrieves the given instance of Greeting
  println(greeting.greet("Hello, Scala 3"))    // Outputs: Hello, Scala 3!
  val intShow = summon[Show[Int]]
  val stringShow = summon[Show[String]]

  println(intShow.show(42))        // Outputs: Int: 42
  println(stringShow.show("Hi"))  // Outputs: String: H

  printShow(123)           // Outputs: Int: 123
  printShow("Scala 3 FTW") // Outputs: String: Scala 3 FTW

  val intCombine = summon[Combine[Int]]
  val listCombine = summon[Combine[List[Int]]]

  println(intCombine.combine(2, 3))           // Outputs: 5
  println(listCombine.combine(List(1, 2), List(3, 4))) // Outputs: List(4, 6)

  val defaultInt = summon[Default[Int]]
  println(defaultInt.value) // Outputs: 0
  println(getOrDefault[Int])      // Outputs: 0
  println(getOrDefault[String])  // Outputs: Default String

  // This will throw an exception because there's no `Default[Double]`
  try 
    println(getOrDefault[Double])
  catch 
    case ex: Exception => println(s"this is ex for double ${ex.getMessage}")
  combineAndPrint(10,20)

  debugGivenInstances[Show[Int]] // Outputs: Some instance found for Int
  debugGivenInstances[Show[Double]] // Outputs: No instance found    