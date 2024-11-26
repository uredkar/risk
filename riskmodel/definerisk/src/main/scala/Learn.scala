import com.definerisk.core.utils.PrettyPrinter.{*, given}
import scala.compiletime.summonFrom
import scala.reflect.ClassTag
import com.definerisk.core.OptionStrategies.*
import java.time.LocalDate

@main def samTypes = 
  trait Increaser:
    def increase(i: Int ): Int

  def increaseOne(increase: Increaser):Int = 
    increase.increase(1)

  def increaseTwo(increase: Increaser):Int = 
    increase.increase(2)

  println(s"increase one by 7 ${increaseOne(i => i + 7)}")
  println(s"increase two by 7 ${increaseTwo(i => i + 7)}")

def makeRowSeq(row: Int) =
    for col <- 1 to 10 yield
      val prod = (row * col).toString
      val padding = " " * (4 - prod.length)
      padding + prod

  // Returns a row as a string
def makeRow(row: Int) = makeRowSeq(row).mkString

  // Returns table as a string with one row per line
def multiTable() =
  val tableSeq = // a sequence of row strings
    for row <- 1 to 10
    yield makeRow(row)

  tableSeq.mkString("\n")

@main def printMultiplicationTable():Unit = 
    println(multiTable())

class Rational(n: Int, d: Int):
  require(d != 0)
  println(s"Created $n $d")
  val denom = d
  val numer = n

  override def  toString = s"$n/$d"
  def this(n: Int) = this(n,1)

  def + (that: Rational): Rational =
    println(s"add $that")
    Rational(numer * that.denom + that.numer * denom,
            denom * that.denom)

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

extension (x: Int)
  def + (y: Rational) = Rational(x) + y

@main def listFiles():Unit = 
  val filesHere = (new java.io.File("C:/sources/risk/riskmodel/definerisk/src/main/scala")).listFiles
  for 
    file <- filesHere
    if file.isFile
    if file.getName.endsWith(".scala")
  do println(file)

@main def rationalExample():Unit = 
  val r = Rational(10,3)
  println(s"r = $r")
  val oneHalf = Rational(1, 2)  
  val twoThirds = Rational(2, 3)  
  val n1 = oneHalf + twoThirds  
  val n2 = twoThirds + oneHalf
  val n3 = 10 + twoThirds
  println(s"onehalf $oneHalf $n1 $n2 $n3")

trait Greeting:
  def sayHello(): String

given Greeting with
  def sayHello(): String = "Hello, Scala 3!"

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

trait Combine[T]:
  def combine(a: T, b: T): T

given Combine[Int] with
  def combine(a: Int, b: Int): Int = a + b

given Combine[String] with
  def combine(a: String, b: String): String = a + b

given [T](using combine: Combine[T]): Combine[List[T]] with
  def combine(a: List[T], b: List[T]): List[T] =
    (a zip b).map((x, y) => summon[Combine[T]].combine(x, y))

trait Default[T]:
  def value: T

given Default[Int] with
  def value: Int = 0

given Default[String] with
  def value: String = "Default String"
/*
inline def getOrDefault[T](using opt: Option[Default[T]]): T =
  //opt.map(_.value).getOrElse(throw new Exception("No default available"))
  summonFrom {
    case default: Default[T] => default.value
    case _                   => throw new Exception(s"No default available for type")
  }

*/

// Automatically create an Option[Default[T]] when a Default[T] is available
given optionDefault[T](using default: Default[T]): Option[Default[T]] = Some(default)

// Fallback to None if no Default[T] is available
given noDefault[T]: Option[Default[T]] = None

// Method to get or provide default value
def getOrDefault[T](using opt: Option[Default[T]]): Option[T] =
  opt.map(_.value)

def combineAndPrint[T](x: T, y: T)(using Show[T]): Unit =
  val showInstance = summon[Show[T]] // Explicitly retrieve the `Show` instance
  println(showInstance.show(x) + " & " + showInstance.show(y))



// Debug method using Option[Show[T]]
def debugGivenInstances[T](using opt: Option[Show[T]], ct: ClassTag[T]): String =
  opt match
    case Some(show) => s"Show instance found: ${show.show(defaultValueFor[T])}"
    case None       => s"No Show instance found for type ${ct.runtimeClass.getName}"

// Provide a default value for testing purposes
def defaultValueFor[T]: T =
  (null.asInstanceOf[T]) // Replace with proper default logic if needed

// Provide a default value for testing purposes
def defaultValueFor[T](using ct: ClassTag[T]): T =
  if (ct.runtimeClass.isPrimitive) 0.asInstanceOf[T]
  else null.asInstanceOf[T]


@main def summonExample(): Unit =
  val greeting = summon[Greeting] // Retrieves the given instance of Greeting
  println(greeting.sayHello())    // Outputs: Hello, Scala 3!
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