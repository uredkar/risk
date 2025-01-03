
trait Animal:
  val name: String 
  override def toString: String = s"Animal($name)"

case class Dog(name: String) extends Animal
case class Cat(name: String) extends Animal

// Covariant declaration
class Box[+A](val content: A) {
  def getContent: A = content
  //def setContent(a: A) = ??? not allowed to modify due to covariant i.e +A
}

class AnyAnimalBox[+A <: Animal](val content: A) {
  def getContent: A = content
}
def printAnimal(box: AnyAnimalBox[Animal]): Unit =
  println(s"Animal: ${box.getContent}")

// Function accepting Box[Animal] so will accept Dog or a Cat
def printAnimal(box: Box[Animal]): Unit =
  println(s"Animal: ${box.getContent}")

val dogBox: Box[Dog] = new Box(Dog("Buddy"))
val catBox: Box[Cat] = new Box(Cat("Whiskers"))

// Covariance allows Box[Dog] and Box[Cat] to be treated as Box[Animal]
printAnimal(dogBox) 
printAnimal(catBox) 


//animalBox.setContent(Cat("Whiskers")) // This would be illegal!

val dogAnimalBox: AnyAnimalBox[Dog] = new AnyAnimalBox(Dog("Buddy"))
val catAnimalBox: AnyAnimalBox[Cat] = new AnyAnimalBox(Cat("Whiskers"))
  
println(dogAnimalBox.getContent) // Dog(Buddy)
println(catAnimalBox.getContent) // Cat(Whiskers)

printAnimal(dogAnimalBox) 
printAnimal(catAnimalBox) 

// invariant can have setter and gettters
class InvariantBox[A <: Animal](private var _content: A) {
  def getContent: A = _content
  def setContent(newContent: A): Unit = _content = newContent
}

 val animalBox: InvariantBox[Animal] = new InvariantBox(Dog("Buddy"))
  println(animalBox.getContent) 

  
  animalBox.setContent(Cat("Whiskers"))
  println(animalBox.getContent) 

// contravariant 
trait Processor[-A] {
  def process(animal: A): Unit
}

val animalProcessor: Processor[Animal] = new Processor[Animal] {
  def process(animal: Animal): Unit =
    println(s"Processing animal: ${animal.name}")
}

val dogProcessor: Processor[Dog] = animalProcessor
dogProcessor.process(Dog("Buddy")) // Processing animal: Buddy