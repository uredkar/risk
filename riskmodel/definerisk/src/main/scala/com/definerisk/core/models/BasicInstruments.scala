package com.definerisk.core.models

import java.time.LocalDate
import scala.language.implicitConversions
import scala.math.BigDecimal
import  com.definerisk.core.utils.PrettyPrinter.{*,given}

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

object Underlying {

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
    override val underlyingType: TradeType,
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

enum PositionType:
  case Long, Short

sealed trait Trade

object Trade:
  case class OptionTrade(
    action: PositionType,     // long or short
    optionType: OptionType, // call or put
    expiry: LocalDate,     // Expiration Date
    strike: BigDecimal,     // Strike Price
    premium: BigDecimal,     // Option Premium
    quantity: Int
  ) extends Trade

  case class StockTrade(
    action: PositionType,     
    price: Double,     
    quantity: Int
  ) extends Trade

  case class ETFTrade(
    action: PositionType,     
    price: Double,     
    quantity: Int
  ) extends Trade

case class Strategy(
    context: Context,
    trades: List[Trade] = List()
) 

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

given PrettyPrinter[Strategy] with
  def prettyPrint(strategy: Strategy): String =
    s"Printing Strategy ----------------------\n" + 
    s"name ${strategy.context.name} difficulty ${strategy.context.difficulty} \n" +
    s"underlying ${strategy.context.underlying} outlook ${strategy.context.outlook} \n" +
    s"strategyType ${strategy.context.strategyType}\n" +
    s"maxRisk ${strategy.context.maxRisk}\n" +
    s"trades ${strategy.trades}\n" + 
    s"End Strategy ----------------------\n"