package com.definerisk.dsl
import scala.collection.mutable
import java.time.LocalDate
import scala.collection.mutable
import java.io.{File, FileWriter, PrintWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import com.definerisk.core.models.{*,given}
import cats.data.Validated
import cats.Functor
import cats.implicits._
import cats.free.Free
import cats.Id
import cats.~>


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



// Portfolio Interpreter
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


// Test the DSL and Free Monad
@main def testPortfolioDSLFreeMonad(): Unit = {
  val account1 = Account(name = "Account1", brokerName = "BrokerA", securities = List.empty)
  val account2 = Account(name = "Account2", brokerName = "BrokerB", securities = List.empty)

  // Create the program
  val program: PortfolioDSL[Unit] = for {
    _ <- PortfolioDSL.createAccount(account1)
    _ <- PortfolioDSL.addSecurity("Account1", ETF("SPY", 20, 400.0))
  } yield ()

  println("Running Free Monad Program")

  program.foldMap(PortfolioDSLInterpreter)
}
