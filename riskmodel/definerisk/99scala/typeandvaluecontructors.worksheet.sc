
// type constructor that operates on type constructors
// functors type contructors operates on List[_] type constructors
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// this is a value contructor as it creates an instance of the type
class Dog(name: String, age: Int) // Constructor parameters

val dog: Dog = new Dog("Buddy", 5) // Creates a Dog instance

// Type constructor: Box is parametrized with a type
class Box[A](val content: A)

// Companion object with value constructor
object Box {
  // Value constructor to create a Box instance
  def apply[A](content: A): Box[A] = new Box(content)
}

val intBox: Box[Int] = Box(42)
val stringBox: Box[String] = Box("Hello")


// Example with List
given Functor[List] with {
  def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
}

// Using both type and value constructors
val list: List[Int] = List(1, 2, 3) 
val result: List[String] = summon[Functor[List]].map(list)(_.toString)

val result2: List[String] = summon[Functor[List]].map(list)(">"+_.toString+"<")
println(s"result2 $result2")

// Functor without given and summon
// Explicit Functor instance for List
val listFunctor: Functor[List] = new Functor[List] {
  def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
}


// Function that takes a Functor instance explicitly
def transform[F[_], A, B](fa: F[A], f: A => B, functor: Functor[F]): F[B] = {
  functor.map(fa)(f)
}

// Applying it to a List
val nums = List(1, 2, 3)
val result3 = transform(nums, (x: Int) => x * 2, listFunctor)

println(result3) // Output: List(2, 4, 6) 


// Items in the SchoolBag
enum SchoolItem:
  case Notebook, Pencil, Pen

// Items in the OfficeBriefCase
enum OfficeItem:
  case Pen, File

// Custom container type: SchoolBag
class SchoolBag[A](val items: List[A]) {
  def transform[B](f: A => B): SchoolBag[B] =
    new SchoolBag(items.map(f))
  override def toString: String = s"SchoolBag(${items.mkString(", ")})"
}

// Custom container type: OfficeBriefCase
class OfficeBriefCase[A](val items: List[A]) {
  def transform[B](f: A => B): OfficeBriefCase[B] =
    new OfficeBriefCase(items.map(f))
  override def toString: String = s"OfficeBriefCase(${items.mkString(", ")})"
}

// Functor type class
trait FunctorTransFormItems[F[_]]:
  def transformItems[A, B](fa: F[A])(f: A => B): F[B]

  // Functor instance for SchoolBag
given FunctorTransFormItems[SchoolBag] with
  def transformItems[A, B](fa: SchoolBag[A])(f: A => B): SchoolBag[B] =
    fa.transform(f)

// Functor instance for OfficeBriefCase
given FunctorTransFormItems[OfficeBriefCase] with
  def transformItems[A, B](fa: OfficeBriefCase[A])(f: A => B): OfficeBriefCase[B] =
    fa.transform(f)

// Extension method for Functor
extension [F[_]: FunctorTransFormItems, A](fa: F[A])
  def transformTo[B](f: A => B): F[B] =
    summon[FunctorTransFormItems[F]].transformItems(fa)(f)

// Create a SchoolBag with SchoolItems
val schoolBag = new SchoolBag(List(SchoolItem.Notebook, SchoolItem.Pencil, SchoolItem.Pen))
println(s"Original SchoolBag: $schoolBag")

// Convert SchoolBag to OfficeBriefCase
val officeBag = schoolBag.transformTo {
    case SchoolItem.Notebook => OfficeItem.File
    case SchoolItem.Pencil   => OfficeItem.File
    case SchoolItem.Pen      => OfficeItem.Pen
}   

println(s"Converted OfficeBriefCase: $officeBag")

  // Convert OfficeBriefCase back to SchoolBag
val backToSchoolBag = officeBag.transformTo {
    case OfficeItem.File => SchoolItem.Notebook // Example mapping
    case OfficeItem.Pen  => SchoolItem.Pen
}

println(s"Converted Back to SchoolBag: $backToSchoolBag")


def transformAll[F[_]: FunctorTransFormItems, A, B](fa: List[F[A]], f: A => B): List[F[B]] =
  fa.map(_.transformTo(f))
