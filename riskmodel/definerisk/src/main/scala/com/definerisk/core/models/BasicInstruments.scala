package com.definerisk.core.models

import java.time.LocalDate
import scala.language.implicitConversions
import scala.math.BigDecimal
import java.io.PrintWriter
import scala.math._
import  com.definerisk.core.utils.PrettyPrinter.{*,given}
import java.time.temporal.ChronoUnit


trait PrinterWriter:
  def println(s: String): Unit

given PrinterWriter with
  def println(s: String): Unit = System.out.println(s)

//case class Underlying(symbol: String, price: BigDecimal, date: LocalDate)
trait UnderlyingType[T <: TradeType]

object UnderlyingType:
  given UnderlyingType[TradeType.Stock.type] with {}
  given UnderlyingType[TradeType.ETF.type] with {}

sealed trait Underlying {
  type T <: Trade
  val underlyingType: TradeType
  val symbol: String
  val price: BigDecimal
  val date: LocalDate
}



case class StockUnderlying(
    underlyingType: TradeType,
    symbol: String,
    price: BigDecimal,
    date: LocalDate
) extends Underlying {
  type T = Trade.StockTrade
}

case class ETFUnderlying(
    underlyingType: TradeType,
    symbol: String,
    price: BigDecimal,
    date: LocalDate
) extends Underlying {
  type T = Trade.ETFTrade
}



enum TradeType:
  case Call, Put, Stock, ETF

sealed trait OptionType:
  def tradeType: TradeType

object OptionType:
  case object Call extends OptionType:
    val tradeType: TradeType = TradeType.Call

  case object Put extends OptionType:
    val tradeType: TradeType = TradeType.Put
      
enum Moneyness:
  case OTM, ITM

enum Strike:
  case LowerStrike, LowerMiddleStrike, HigherMiddleStrike, higherStrike

enum Action:
    case Buy, Sell

enum ExpiryDate:
    case Same

enum Distance:
    case Equidistance

enum StrategyType:
    case CapitalGain, Income, Vertical, Volatility, Sideways, Leveraged, Synthetic

object  StrategyType {
  def fromString(str: String): Option[StrategyType] = str.trim.toLowerCase match {
    case "capitalgain" => Some(CapitalGain)
    case "income" => Some(Income)
    case _ => None
  }
}
enum Risk:
    case CappedRisk, UnCappedRisk, CappedReward, UnCappedReward

enum Proficiency:
    case Novice, Intermediate, Advanced, Expert

enum Greeks:
    case Gamma, Theta, Vega, Rho

case class TradeGreeks(
  delta: BigDecimal,
  gamma: BigDecimal,
  vega: BigDecimal,
  rho: BigDecimal,
  theta: BigDecimal
)

enum PositionType:
  case Long, Short

sealed trait Trade

object Trade:
  case class OptionTrade(
    transactionId: String,
    transactionDate: LocalDate,
    symbol: String,
    action: PositionType,     // long or short
    optionType: OptionType, // call or put
    expiry: LocalDate,     // Expiration Date
    timeToExpiry: BigDecimal,
    strike: BigDecimal,     // Strike Price
    premium: BigDecimal,     // Option Premium
    quantity: Int
  ) extends Trade

  case class StockTrade(
    transactionId: String,
    transactionDate: LocalDate,
    symbol: String,
    action: PositionType,     
    price: BigDecimal,     
    quantity: Int
  ) extends Trade

  case class ETFTrade(
    transactionId: String,
    transactionDate: LocalDate,
    symbol: String,
    action: PositionType,     
    price: BigDecimal,     
    quantity: Int
  ) extends Trade

  object OptionTrade {

  // Factory method with a given `currentDate`
  def apply(
      transactionId: String,
      transactionDate: LocalDate,
      symbol: String,
      action: PositionType,
      optionType: OptionType,
      expiry: LocalDate,
      strike: BigDecimal,
      premium: BigDecimal,
      quantity: Int
  )(using currentDate: LocalDate): Trade.OptionTrade = {
    val daysToExpiry = ChronoUnit.DAYS.between(currentDate, expiry)
    val timeToExpiry = BigDecimal(daysToExpiry) / 365 // Convert days to years
    OptionTrade(transactionId,transactionDate,symbol,action, optionType, expiry, timeToExpiry, strike, premium, quantity)
  }

  // Default given instance for `currentDate`
  given LocalDate = LocalDate.now()
}




case class OptionLeg(
  legId: String,
  trades: List[Trade]
)

case class Strategy(
    strategyId: String,
    context: Context,
    //trades: List[Trade] = List()
    legs: List[OptionLeg]
) 


case class Account(
    name: String,
    brokerName: String,
    securities: List[Security]
)

case class Portfolio(accounts: List[Account])
sealed trait Security:
  def symbol: String
  def quantity: Int
  def currentPrice: BigDecimal
  def calculateValue: BigDecimal = quantity * currentPrice

case class Stock(symbol: String, quantity: Int, currentPrice: BigDecimal) extends Security
case class Bond(symbol: String, quantity: Int, currentPrice: BigDecimal,yieldPercentage: BigDecimal, parValue: BigDecimal, maturityDate: LocalDate) extends Security
case class ETF(symbol: String, quantity: Int, currentPrice: BigDecimal) extends Security
case class StockOption(symbol: String, quantity: Int, currentPrice: BigDecimal,optionType: OptionType, strikePrice: BigDecimal, premium: BigDecimal, expiryDate: LocalDate) extends Security
  
case class Context(
  name: String,
  difficulty: String,
  direction: String,
  outlook: Option[String] = None,
  maxReward: Option[String] = None,
  maxRisk: Option[String] = None,
  breakEvenDown: Option[String] = None,
  strategyType: Option[StrategyType] = None,
  volatility: String,
  underlying: Option[Underlying]
)


trait MyContainer[F[_]] {
    def get[A](fa: F[A]): A
}

given listContainer: MyContainer[List] with {
    def get[A](fa: List[A]): A = fa.head
}

given PrettyPrinter[Strategy] with
  def prettyPrint(strategy: Strategy): String =
    s"Printing Strategy ${strategy.strategyId} ----------------------\n" + 
    s"name ${strategy.context.name} difficulty ${strategy.context.difficulty} \n" +
    s"underlying ${strategy.context.underlying} outlook ${strategy.context.outlook} \n" +
    s"strategyType ${strategy.context.strategyType}\n" +
    s"maxRisk ${strategy.context.maxRisk}\n" +
    s"trades ${strategy.legs}\n" + 
    s"End Strategy ----------------------\n"


