import com.definerisk.core.models.{*,given}
import com.definerisk.core.utils.YamlUtil.{*,given}
import cats.data.Validated
import cats.Functor
import cats.implicits._
import cats.free.Free
import cats.Id
import cats.~>
import com.definerisk.core.dsl.{*,given}
import java.io.PrintWriter
import java.time.LocalDate
import java.time.format.DateTimeFormatter


import java.time.LocalDate
import scala.collection.mutable

import java.io.{File, FileWriter, PrintWriter}

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

/*
sealed trait Free[F[_], A]:
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free.FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(a => Free.Pure(f(a)))

object Free:
  case class Pure[F[_], A](value: A) extends Free[F, A]
  case class Suspend[F[_], A](effect: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](sub: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def pure[F[_], A](value: A): Free[F, A] = Pure(value)
  def liftF[F[_], A](effect: F[A]): Free[F, A] = Suspend(effect)
*/
//type TradeFree[A] = Free[TradeOp, A]



sealed trait PortfolioOp[A]
object PortfolioOp:
  case class CreateAccount(account: Account) extends PortfolioOp[Unit]
  case class AddSecurity(accountName: String, security: Security) extends PortfolioOp[Unit]
  case class RemoveSecurity(accountName: String, symbol: String) extends PortfolioOp[Unit]
  case class ModifySecurity(accountName: String, symbol: String, updatedSecurity: Security) extends PortfolioOp[Unit]
  case class SummarizeByAccount(accountName: String) extends PortfolioOp[String]
  case class SummarizePortfolio() extends PortfolioOp[String]
  case class SavePortfolio(filePath: String) extends PortfolioOp[Unit]
  case class LoadPortfolio(filePath: String) extends PortfolioOp[Portfolio]
  case class ExecuteTrade(accountName: String, trade: Trade) extends PortfolioOp[Unit]
  case class GetPortfolioSummary() extends PortfolioOp[String]

type PortfolioDSL[A] = Free[PortfolioOp, A]

object PortfolioDSL {
  def createAccount(account: Account): PortfolioDSL[Unit] =
    Free.liftF(PortfolioOp.CreateAccount(account))
  
  def addSecurity(accountName: String, security: Security): PortfolioDSL[Unit] =
    Free.liftF(PortfolioOp.AddSecurity(accountName, security))
}

object PortfolioDSLInterpreter extends (PortfolioOp ~> Id) {
  def apply[A](fa: PortfolioOp[A]): Id[A] = fa match {
    case PortfolioOp.CreateAccount(account) =>
      // Perform the operation
      println(s"createAccount $account")
      ()
    case PortfolioOp.AddSecurity(accountName, security) =>
      // Perform the operation
      println(s"AddSecurity $accountName $security")
      ()
  }
}


def test_dsl_free_monad() =
  import PortfolioDSL._
  val program: PortfolioDSL[Unit] = for {
    _ <- createAccount(Account("Account1", "BrokerA", List.empty))
    _ <- addSecurity("Account1", Stock("AAPL", 10, 150.0))
  } yield ()
  program.foldMap(PortfolioDSLInterpreter)

class PortfolioInterpreter:
  import YamlUtil.{given}

  private val portfolioData = mutable.Map[String, Account]()
  private var portfolio = Portfolio(accounts = List.empty)

  def interpret[A](op: PortfolioOp[A]): A = op match {
    case PortfolioOp.CreateAccount(account) =>
      portfolio = portfolio.copy(accounts = portfolio.accounts :+ account)
      println(s"Account '${account.name}' created.")
      portfolioData.put(account.name,account)
      ().asInstanceOf[A]
    
    case PortfolioOp.AddSecurity(accountName, security) =>
      
      portfolio = portfolio.copy(accounts = portfolio.accounts.map { account =>
        if (account.name == accountName) {
          println(s"AddSecurity2 $accountName $security")
          account.copy(securities = account.securities :+ security)
        }
        else account
      })
      println(s"Security '${security}' added to account '$accountName'.")
      ().asInstanceOf[A]

    case PortfolioOp.RemoveSecurity(accountName, symbol) =>
      portfolio = portfolio.copy(accounts = portfolio.accounts.map { account =>
        if (account.name == accountName)
          account.copy(securities = account.securities.filterNot(_.symbol == symbol))
        else account
      })
      println(s"Security '$symbol' removed from account '$accountName'.")
      ().asInstanceOf[A]

    case PortfolioOp.GetPortfolioSummary() =>
      val summary = portfolio.accounts.map { account =>
        val totalValue = account.securities.map(_.calculateValue).sum
        s"Account: ${account.name}, Broker: ${account.brokerName}, Total Value: $$${totalValue}"
      }.mkString("\n")
      println("Portfolio Summary Generated.")
      summary.asInstanceOf[A]
  
    case PortfolioOp.SavePortfolio(filePath) =>
      YamlUtil.savePortfolio(portfolio, new File(filePath))
      println(s"Portfolio saved to $filePath.")
      ().asInstanceOf[A]
  
    case PortfolioOp.LoadPortfolio(filePath) =>
      portfolio = YamlUtil.loadPortfolio(new File(filePath)).getOrElse {
        println(s"Failed to Load Portfolio $filePath")
        Portfolio(accounts = List.empty)
      }
      println(s"Portfolio loaded from $filePath.")
      ().asInstanceOf[A]

    case PortfolioOp.ExecuteTrade(accountName, trade) =>
      portfolioData.get(accountName) match {
        case Some(account) =>
          val updatedSecurities = trade match {
            case Trade.StockTrade(_, _,symbol, action, price, quantity) =>
              val delta = if action == PositionType.Long then quantity else -quantity
              updateSecurity(account, Stock(symbol, delta, price)) // Replace "AAPL" with trade's symbol
            
            case Trade.OptionTrade(_, _, symbol,action, optionType, expiry, _, strike, premium,quantity) =>
              val delta = if action == PositionType.Long then quantity else -quantity
              updateSecurity(account, StockOption(symbol,delta,premium, optionType, strike, premium,expiry)) // fix premium vs current price tbd
            case Trade.ETFTrade(_, _,symbol, action, price, quantity) =>
              val delta = if action == PositionType.Long then quantity else -quantity
              updateSecurity(account, ETF(symbol, delta, price)) // Replace "SPY" with trade's symbol
            
            case _ => account.securities // For unsupported trade types, leave securities as-is
          }
          portfolioData(accountName) = account.copy(securities = updatedSecurities)

        case None => println(s"ExecuteTrade Account $accountName not found!")
      }

    case _ => {
      println("interpret not handled") 
      throw new NoSuchElementException("empty list")
    }
  }

  private def updateSecurity(account: Account, newSecurity: Security): List[Security] = {
  val (matching, others) = account.securities.partition {
    case s: Stock => newSecurity match {
      case ns: Stock => s.symbol == ns.symbol
      case _         => false
    }
    case o: StockOption => newSecurity match {
      case nso: StockOption => o.symbol == nso.symbol
      case _                => false
    }
    case e: ETF => newSecurity match {
      case ne: ETF => e.symbol == ne.symbol
      case _       => false
    }
    case _ => false
  }

  val updated = matching match {
    case Nil => List(newSecurity)
    case head :: _ => (head, newSecurity) match {
      case (s: Stock, ns: Stock) =>
        List(s.copy(quantity = s.quantity + ns.quantity, currentPrice = ns.currentPrice))
      case (o: StockOption, nso: StockOption) =>
        List(o.copy(quantity = o.quantity + nso.quantity))
      case (e: ETF, ne: ETF) =>
        List(e.copy(quantity = e.quantity + ne.quantity, currentPrice = ne.currentPrice))
      case _ => List(newSecurity)
    }
  }

  others ++ updated
  }



@main def freeMonadPortfolio(): Unit = 
  val account1 = Account(name = "Account1", brokerName = "BrokerA", securities = List.empty)
  val account2 = Account(name = "Account2", brokerName = "BrokerB", securities = List.empty)

  
  val createAccountOp = PortfolioOp.CreateAccount(account1)
  println("AddSecurity.........................")
  val securityOp1 = PortfolioOp.AddSecurity(account1.name, ETF("SPY", 20, 400.0))
  val securityOp2 = PortfolioOp.AddSecurity(account1.name, StockOption("TSLA", 10, 50.0, OptionType.Call, 750.0, 5.0, LocalDate.of(2024, 6, 30)))
  val securityOp3 = PortfolioOp.AddSecurity(account2.name, Stock("AAPL", 100, 150.0))
  val createAnotherAccountOp = PortfolioOp.CreateAccount(account2)

  // Execute trades for stocks, options, and ETFs
  val stockTrade = Trade.StockTrade(
    transactionId = "ST123",
    transactionDate = LocalDate.now(),
    symbol = "APPL",
    action = PositionType.Long,
    price = BigDecimal(200.0),
    quantity = 50
  )

  val optionTrade = Trade.OptionTrade(
    transactionId = "OT123",
    transactionDate = LocalDate.now(),
    symbol = "APPL000000",
    action = PositionType.Short,
    optionType = OptionType.Call,
    expiry = LocalDate.now().plusMonths(1),
    timeToExpiry = BigDecimal(0.083), // Approximately 1/12 year
    strike = BigDecimal(210.0),
    premium = BigDecimal(5.0),
    quantity = 10
  )

  
  val executeOptionTradeOp = PortfolioOp.ExecuteTrade("Account1", optionTrade)
  val executeStockTradeOp = PortfolioOp.ExecuteTrade("Account2", stockTrade)
  

  // Save and load portfolio
  
  

  // Usage with PortfolioInterpreter
  val portfolioInterpreter = new PortfolioInterpreter()

  portfolioInterpreter.interpret(createAccountOp)
  portfolioInterpreter.interpret(createAnotherAccountOp)
  portfolioInterpreter.interpret(securityOp1)
  portfolioInterpreter.interpret(securityOp2)
  portfolioInterpreter.interpret(securityOp3)
  portfolioInterpreter.interpret(executeStockTradeOp)
  portfolioInterpreter.interpret(executeOptionTradeOp)
  
  
  val summary = portfolioInterpreter.interpret(PortfolioOp.GetPortfolioSummary())
  println(summary)
  val savePortfolioOp = PortfolioOp.SavePortfolio("portfolio2.yaml")
  portfolioInterpreter.interpret(savePortfolioOp)
  val loadPortfolioOp = PortfolioOp.LoadPortfolio("portfolio2.yaml")
  println("--------- reading portfolio from yaml.....")
  portfolioInterpreter.interpret(loadPortfolioOp)
  println("--------- ")

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
                          case Trade.OptionTrade(transactionId, transactionDate, symbol,action, optionType, expiry, timeToExpiry,strike, premium, quantity) => transactionDate
                          case Trade.StockTrade(_,transactionDate,symbol, _, _, _) => transactionDate
                      } // Sort trades by transaction date

                    flattenedAndSortedTrades.foreach {
                      case Trade.OptionTrade(transactionId, transactionDate, symtbol, action, optionType, expiry, timeToExpiry,strike, premium, quantity) =>
                       writer.println(s"$transactionId,$transactionDate, $action, $optionType,  $expiry,$strike,$premium,$quantity")
                      case Trade.StockTrade(transactionId, transactionDate, symbol,action, price, quantity) =>
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
    test_dsl_free_monad()