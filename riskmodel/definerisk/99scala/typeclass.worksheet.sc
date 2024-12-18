case class Person(name: String, age: Int)
val list = List(1, 2, 3,4)
val result = list.mkString("List[]", "| ", "[]]")
println(result)

val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
val resultMap = map.mkString("Mappp(", "||||", ")")
println(resultMap)


trait Serializer[T]:
  def serialize(value: T): String
  
given Serializer[Person] with
  def serialize(person: Person): String =
    s"Person(name=${person.name}, age=${person.age})"

given Serializer[Int] with
  def serialize(value: Int): String = s"Int($value)"

given Serializer[String] with
  def serialize(value: String): String = s"String($value)"

// this method can serialize Int, String or List of Int or List of String
def toSerializedString[T](value: T)(using serializer: Serializer[T]): String =
  serializer.serialize(value)

given [T](using serializer: Serializer[T]): Serializer[List[T]] with
  def serialize(value: List[T]): String =
    value.map(serializer.serialize).mkString("List(", ", ", ")")

given [K, V](using keySerializer: Serializer[K], valueSerializer: Serializer[V]): Serializer[Map[K, V]] with
  def serialize(value: Map[K, V]): String =
    value.map { case (k, v) =>
      s"${keySerializer.serialize(k)} -> ${valueSerializer.serialize(v)}"
    }.mkString("Map(", ", ", ")")

trait Container {
  type Element
  def addElement(e: Element): Unit
  def getElements: List[Element]
}

class IntContainer extends Container {
  type Element = Int  // Specify the type member
  private var elements = List.empty[Element]

  def addElement(e: Element): Unit = elements = e :: elements
  def getElements: List[Element] = elements
}

class StringContainer extends Container {
  type Element = String
  private var elements = List.empty[Element]

  def addElement(e: Element): Unit = elements = e :: elements
  def getElements: List[Element] = elements
}  

type StringMap = Map[String, String]

trait Logger {
  type LogLevel = String  // Type alias within a trait
  def log(level: LogLevel, message: String): Unit
}

class ConsoleLogger extends Logger {
  def log(level: LogLevel, message: String): Unit =
    println(s"[$level]: $message")
}

println(toSerializedString(42))         
println(toSerializedString("Hello"))   

val person = Person("Alice", 30)
println(toSerializedString(person)) 

println(toSerializedString(List(1, 2, 3)))          
println(toSerializedString(List("a", "b", "c")))    

val people = List(Person("Alice", 30), Person("Bob", 25))
println(toSerializedString(people))

 val peopleMap = Map("GroupA" -> List(Person("Alice", 30)), "GroupB" -> List(Person("Bob", 25)))
  println(toSerializedString(peopleMap))



 val intContainer = new IntContainer
intContainer.addElement(42)
println(intContainer.getElements) // List(42)

val stringContainer = new StringContainer
stringContainer.addElement("Hello")
println(stringContainer.getElements) // List(Hello)

val logger = new ConsoleLogger
logger.log("INFO", "Application started") // [INFO]: Application started