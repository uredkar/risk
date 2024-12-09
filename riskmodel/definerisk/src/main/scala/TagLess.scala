import cats.{Id, Monad}
import cats.syntax.flatMap._
import cats.syntax.functor._

// AccountAlgebra trait remains unchanged
trait AccountAlgebra[F[_]]:
  def createAccount(name: String, initialBalance: Double): F[Unit]
  def getBalance(name: String): F[Double]
  def deposit(name: String, amount: Double): F[Unit]
  def withdraw(name: String, amount: Double): F[Unit]

// InMemoryAccountInterpreter implementation
class InMemoryAccountInterpreter[F[_]: Monad] extends AccountAlgebra[F]:
  // state is here!
  private val accounts = scala.collection.mutable.Map.empty[String, Double]

  def createAccount(name: String, initialBalance: Double): F[Unit] =
    Monad[F].pure(accounts.put(name, initialBalance).fold(())(_ => ()))

  def getBalance(name: String): F[Double] =
    Monad[F].pure(accounts.getOrElse(name, 0.0))

  def deposit(name: String, amount: Double): F[Unit] =
    accounts.updateWith(name)(_.map(_ + amount))
    Monad[F].pure(())

  def withdraw(name: String, amount: Double): F[Unit] =
    accounts.updateWith(name)(_.map(_ - amount))
    Monad[F].pure(())

class LoggingAccountInterpreter[F[_]: Monad](inner: AccountAlgebra[F]) extends AccountAlgebra[F]:
  def createAccount(name: String, initialBalance: Double): F[Unit] =
    for
      balance <- inner.createAccount(name,initialBalance)
      _ <- Monad[F].pure(println(s"createAccount for $name: $initialBalance"))
    yield balance

  def getBalance(account: String): F[Double] =
    for
      balance <- inner.getBalance(account)
      _ <- Monad[F].pure(println(s"Checked balance for $account: $balance"))
    yield balance

  def deposit(account: String, amount: Double): F[Unit] =
    for
      _ <- inner.deposit(account, amount)
      _ <- Monad[F].pure(println(s"Deposited $amount to $account"))
    yield ()

  def withdraw(account: String, amount: Double): F[Unit] =
    for
      _ <- inner.withdraw(account, amount)
      _ <- Monad[F].pure(println(s"Withdrew $amount from $account"))
    yield ()

class RemoteAccountInterpreter[F[_]: Monad] extends AccountAlgebra[F]:
  def createAccount(name: String, initialBalance: Double): F[Unit] =
    Monad[F].pure {
      println(s"createAccount for $name: $initialBalance")
      
    }
  def getBalance(account: String): F[Double] =
    Monad[F].pure {
      println(s"Fetching balance for $account from remote service")
      1000.0 // Simulate remote balance
    }

  def deposit(account: String, amount: Double): F[Unit] =
    Monad[F].pure {
      println(s"Depositing $amount to $account via remote service")
    }

  def withdraw(account: String, amount: Double): F[Unit] =
    Monad[F].pure {
      println(s"Withdrawing $amount from $account via remote service")
    }

// Transfer function
def transfer[F[_]: Monad](using algebra: AccountAlgebra[F])(
    from: String,
    to: String,
    amount: Double
): F[Unit] =
  for
    balance <- algebra.getBalance(from)
    _ <- if balance >= amount then
      for
        _ <- algebra.withdraw(from, amount)
        _ <- algebra.deposit(to, amount)
      yield ()
    else Monad[F].pure(println(s"Insufficient funds in account: $from"))
  yield ()

given Monad[Id] with
  def pure[A](a: A): Id[A] = a
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] =
    @annotation.tailrec
    def loop(current: A): Id[B] = f(current) match {
      case Right(b) => b
      case Left(next) => loop(next)
    }
    loop(a)

def program[F[_]: Monad](using algebra: AccountAlgebra[F]) =
    for
      _ <- algebra.createAccount("Alice", 1000.0)
      _ <- algebra.createAccount("Bob", 500.0)
      _ <- transfer[F]("Alice", "Bob", 300.0)
      _ <- transfer[F]("Alice", "Bob", 800.0)
    yield ()

def runWithInterpreter[F[_]: Monad](interpreter: AccountAlgebra[F], description: String): Unit =
  println(s"\n=== $description ===")
  given AccountAlgebra[F] = interpreter
  program[F]    
// Main Program
@main def taglessExample() =
  
  // InMemory Interpreter
  val inMemoryInterpreter: AccountAlgebra[Id] = InMemoryAccountInterpreter[Id]()
  runWithInterpreter(inMemoryInterpreter, "InMemory Interpreter")

  // Logging Interpreter
  val loggingInterpreter: AccountAlgebra[Id] = LoggingAccountInterpreter(inMemoryInterpreter)
  runWithInterpreter(loggingInterpreter, "Logging Interpreter")
  
  