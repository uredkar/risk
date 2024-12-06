import com.definerisk.core.models.*
import cats.data.Validated
import cats.Functor
import cats.implicits._
import com.definerisk.core.dsl.{*,given}
import java.io.PrintWriter

trait MyContainer[F[_]] {
    def get[A](fa: F[A]): A
}

given listContainer: MyContainer[List] with {
    def get[A](fa: List[A]): A = fa.head
}

def firstElement[F[_], A](container: F[A])(using MyContainer[F]): A = 
    summon[MyContainer[F]].get(container)

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

object Functor:
  def apply[F[_]](using functor: Functor[F]): Functor[F] = functor

  given Functor[List] with
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

  given Functor[Option] with
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

extension [F[_]](functor: Functor[F]) // Attach extensions to an explicit parameter
  def compose[G[_]](using Functor[G]): Functor[[A] =>> F[G[A]]] =
    new Functor[[A] =>> F[G[A]]]:
      def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
        functor.map(fa)(ga => summon[Functor[G]].map(ga)(f))


def transform[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[B] =
  summon[Functor[F]].map(fa)(f)

trait MyHigherKind[F[_]]

val eitherExample: MyHigherKind[[X] =>> Either[String, X]] = 
  new MyHigherKind[[X] =>> Either[String, X]] {}

def validate[F[_]: Functor, A](fa: F[A]): F[String] =
  summon[Functor[F]].map(fa)(_.toString)

type MyValidated[A] = Validated[List[String], A]

// Example usage
val result = (1.valid[List[String]], 2.valid[List[String]]).mapN(_ + _)


def processData[F[_]: Functor](container: F[Int]): F[Int] =
  summon[Functor[F]].map(container)(_ * 2)

given functorForValidatedNothing: Functor[[A] =>> Validated[Nothing, A]] with
  def map[A, B](fa: Validated[Nothing, A])(f: A => B): Validated[Nothing, B] = 
    fa.map(f)

given Functor[Option] with
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

given Functor[List] with
  def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

  
//type Flatten[F[_]] = [A] =>> F[Option[A]]
type Flatten[F[_]] = [A, B] =>> F[Option[A]] => F[Option[B]]



trait Monad[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // `map` can be derived from `flatMap` and `pure`
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

object Monad:
  given Monad[List] with
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

  given Monad[Option] with
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    

final case class OptionT[F[_], A](value: F[Option[A]]):

  // Lifts a pure value into OptionT
  def pure[B](b: B)(using functor: Functor[F]): OptionT[F, B] =
    OptionT(functor.map(value)(_ => Some(b)))

  // Maps over the inner value
  def map[B](f: A => B)(using functor: Functor[F]): OptionT[F, B] =
    OptionT(functor.map(value)(_.map(f)))

  // FlatMaps over the transformer
  def flatMap[B](f: A => OptionT[F, B])(using monad: Monad[F]): OptionT[F, B] =
    OptionT(monad.flatMap(value) {
      case Some(a) => f(a).value
      case None    => monad.pure(None)
    })


class SafeContainer[F[_]](val data: F[Option[Int]])(using functor: Functor[F]) {
  // Method to map over the inner Option
  def mapInner[B](f: Int => B): F[Option[B]] = {
    functor.map(data) {
        case Some(value) => 
            val transformed : B = f(value)
            println(s"Mapping value: $value to \"$transformed\"") 
            Some(transformed)
        case None => None
    }
  }

  // Method to extract values safely
  def extractOrElse(default: Int): F[Int] = {
    //val functorInstance = summon[Functor[F]]
    //functorInstance.map(data)(_.getOrElse(default))
    functor.map(data)(_.getOrElse(default))
  }
}

  
    
    

@main def reportTrades(): Unit =
    val directory = "./src/main/resources/" // Replace with your directory
    val yamlContents = YamlReader.readYamlFiles(directory)
    val writer = new PrintWriter("flat_report.csv")
    writer.println("transactionId, transactionDate, action, optionType, expiry, timeToExpiry,strike, price, quantity")
    yamlContents.foreach { case (file, content) =>
        println(s"File: $file")
        
        val ctx = DSLProcessor.parse(content) match 
                case Right(strategy) =>  {
                    val flattenedAndSortedTrades = strategy.legs
                      .flatMap(leg => leg.trades)           // Flatten the nested list of trades
                     .sortBy {
                          case Trade.OptionTrade(transactionId, transactionDate, action, optionType, expiry, timeToExpiry,strike, premium, quantity) => transactionDate
                          case Trade.StockTrade(_, transactionDate, _, _, _) => transactionDate
                      } // Sort trades by transaction date

                    flattenedAndSortedTrades.foreach {
                      case Trade.OptionTrade(transactionId, transactionDate, action, optionType, expiry, timeToExpiry,strike, premium, quantity) =>
                       writer.println(s"$transactionId,$transactionDate, $action, $optionType,  $expiry,$strike,$premium,$quantity")
                      case Trade.StockTrade(transactionId, transactionDate, action, price, quantity) =>
                        writer.println(s"$transactionId,$transactionDate, $action,stock,,,,$price,  $quantity")
                    }
                }
                case Left(error) => "Error"
        writer.close()                
        println(ctx)   
    }
    

def test_optionT() = 
    import scala.concurrent.{Future, ExecutionContext}
    import scala.concurrent.ExecutionContext.Implicits.global

    // Define a Functor and Monad for Future
    given Functor[Future] with
        def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

    given Monad[Future] with
        def pure[A](a: A): Future[A] = Future.successful(a)
        def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

    val futureOption: Future[Option[Int]] = Future.successful(Some(42))
    val optionT = OptionT(futureOption)

    // Use map and flatMap with OptionT
    val result = optionT.map(_ + 1).flatMap(x => OptionT(Future.successful(Some(x * 2))))
    println("test_optionT")
    result.value.foreach(println) // Outputs: Some(86)

def test_mycompose() =
    def my_compose[F[_], G[_]](using ff: Functor[F], gf: Functor[G]): Functor[[A] =>> F[G[A]]] =
        new Functor[[A] =>> F[G[A]]]:
            def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
                ff.map(fa)(ga => gf.map(ga)(f))

    given Functor[List] with
        def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    val composedFunctor2 = my_compose[Option, List]
    val nested = Some(List(1, 2, 3))
    val r2 = composedFunctor2.map(nested)(_ * 2)
    println("testing compose")
    println(r2) // Some(List(2, 4, 6)

    val optionListFunctor = Functor[Option].compose[List]

    val nestedStructure: Option[List[Int]] = nested
    val r3 = optionListFunctor.map(nestedStructure)(_ + 10)

    println(r3) // Output: Some(List(11, 12, 13))

def test_typed_lambda() = 
    type PairWith[A] = [B] =>> (A,B)
    type OptionList[A] = [T] =>> Option[List[T] => A]
    
    val pair: PairWith[Int][String] = (42,"hello")
    //val myFunc: OptionList[Int] = (list: List[String]) => list.length * 2
    val myFunc: OptionList[Int][String] = Some((list: List[String]) => list.length * 2)

    println(s"pair $pair")
    val result = myFunc.getOrElse((_: List[String]) => 0)(List("apple", "banana"))
    println(result)
    val listOfOptions: List[Option[Int]] = List(Some(1), None, Some(2))
    
    println(s"list of options $listOfOptions ${listOfOptions.getClass}")
    val listSafeContainer = new SafeContainer[List](List(Some(1), None, Some(2)))

    // Map over the inner Option[Int] to transform values
    val mappedContainer1: List[Option[String]] = listSafeContainer.mapInner[String]("Y->" + _.toString + "<-X")
    
    println("Mapped Container 1")
    println(mappedContainer1) 

    val mappedContainer2: List[Option[String]] = listSafeContainer.mapInner("Y->" + _.toString + "<-X")
    
    println("Mapped Container 2")
    println(mappedContainer2) 
    

    // Extract values with a default fallback
    val extractedContainer: List[Int] = listSafeContainer.extractOrElse(0)
    println("extracted Container")
    println(extractedContainer) // Output: List(1, 0, 2)




@main def testMonads() =     
    println("\nFirst element -------")
    println(firstElement(List(1, 2, 3))) 
    println(transform(List(1, 2, 3))(_ + 1)) 
    println(validate(Validated.valid(42)))
    println(result) 
    println(processData(Option(10))) 
    println(processData(List(1, 2, 3))) 
    val validated: Validated[Nothing, Int] = 42.valid[Nothing]
    val mapped = validated.map(_ + 1) 
    println(mapped) 
    val nested: Option[List[Int]] = Some(List(1, 2, 3))
    

    val r1 = nested.map(_.map(_ * 2))
    println(r1)
    val composedFunctor1 = Functor[Option].compose[List]
    val c1 = composedFunctor1.map(nested)(_ * 2)
    
    println(c1)
    test_mycompose()
    test_typed_lambda()
    test_optionT()