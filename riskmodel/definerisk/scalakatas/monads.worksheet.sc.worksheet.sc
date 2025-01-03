
import $ivy.`org.typelevel::cats-effect:3.5.7`

import cats.effect.unsafe.implicits._
import cats.effect.IO
import cats.Monad
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.OptionT
import cats.data.EitherT
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Either
import cats.Eval
import cats.data.Reader
import cats.data.State
import cats.effect.IO





println(eager.value)  

val lazyMemoized = Eval.later {
  println("Lazy evaluation (memoized)"); 
  42
}

println(lazyMemoized.value) // Prints: Lazy evaluation (memoized)
println(lazyMemoized.value) // No re-evaluation; Prints: 42

val lazyAlways = Eval.always {
  println("Lazy evaluation (not memoized)"); 
  42
}

println(lazyAlways.value) // Prints: Lazy evaluation (not memoized), 42
println(lazyAlways.value) // Re-evaluates; Prints: Lazy evaluation (not memoized), 42

val program1: IO[Unit] = for {
  _ <- IO(println("Enter your name:"))
  name <- IO(scala.io.StdIn.readLine())
  _ <- IO(println(s"Hello, $name!"))
} yield ()

program1.unsafeRunSync()

val increment: State[Int, Int] = State { state =>
  (state + 1, state)
}

val computation = for {
  _ <- increment
  x <- increment
} yield x

println(computation.run(10).value) // (12, 11)

val optionMonad = Monad[Option]
val result = optionMonad.flatMap(Some(10))(x => Some(x * 2))
println(result)

val futureOption1: OptionT[Future, Int] = OptionT(Future.successful(Some(10)))
val futureOption2: OptionT[Future, Int] = OptionT(Future.successful(Some(20)))

val result0: OptionT[Future, Int] = for {
  x <- futureOption1
  y <- futureOption2
} yield x + y

result0.value.foreach(println) 

def safeDivide(a: Int, b: Int): Either[String, Int] = 
  if (b == 0) Left("Division by zero") else Right(a / b)

val result3: Either[String, Int] = for {
  x <- safeDivide(10, 2)
  y <- safeDivide(20, 5)
} yield x + y

println(result3) 

val resultC = for {
  x <- List(Some(10), None, Some(20)).flatten
  y <- List(1, 2, 3)
} yield x * y

println(resultC)

case class Config(base: Int)

val add: Reader[Config, Int] = Reader(config => config.base + 10)
val multiply: Reader[Config, Int] = Reader(config => config.base * 2)

val combined: Reader[Config, Int] = for {
  a <- add
  b <- multiply
} yield a + b

println(combined.run(Config(5)))

val computation2: Eval[Int] = for {
  x <- Eval.later { println("Computing x"); 10 }
  y <- Eval.always { println("Computing y"); x + 20 }
} yield x + y

println(computation2.value) 

val futureEither1: EitherT[Future, String, Int] = EitherT(Future.successful(Right(10)))
val futureEither2: EitherT[Future, String, Int] = EitherT(Future.successful(Right(20)))

val resultT: EitherT[Future, String, Int] = for {
  x <- futureEither1
  y <- futureEither2
} yield x + y

resultT.value.foreach(println) 

def divide(a: Int, b: Int): Option[Int] = 
  if (b == 0) None else Some(a / b)

val result1 = for {
  x <- Some(10)
  y <- Some(2)
  z <- divide(x, y)
} yield z
println(result1) // Some(5)

val result2 = for {
  x <- Future(10)
  y <- Future(20)
} yield x + y

result2.foreach(println) // Asynchronous computation


def divide2(a: Int, b: Int): Either[String, Int] = 
  if (b == 0) Left("Division by zero") else Right(a / b)

val result4 = for {
  x <- Right(10): Either[String, Int]
  y <- Right(2): Either[String, Int]
  z <- divide2(x, y)
} yield z
println(result4) 

 