package com.definerisk.dsl

import java.time.LocalDate
import scala.language.implicitConversions
//import com.definerisk.dsl.Trade.OptionTrade

case class Underlying(symbol: String, price: BigDecimal, date: LocalDate)
// Define Underlying Context
//case class UnderlyingContext(symbol: String, price: Double, date: LocalDate)

enum TradeType:
  case Call, Put, Stock, ETF

sealed trait OptionType:
  def tradeType: TradeType

object OptionType:
  case object Call extends OptionType:
    val tradeType: TradeType = TradeType.Call

  case object Put extends OptionType:
    val tradeType: TradeType = TradeType.Put

sealed trait EquitiesType:
  def tradeType: TradeType

object EquitiesType:
  case object Stock extends EquitiesType:
    val tradeType: TradeType = TradeType.Stock

  case object ETF extends EquitiesType:
    val tradeType: TradeType = TradeType.ETF

enum PositionType:
  case Long, Short

enum StrategyType:
  case CapitalGain, Income

object  StrategyType {
  def fromString(str: String): Option[StrategyType] = str.trim.toLowerCase match {
    case "capitalgain" => Some(CapitalGain)
    case "income" => Some(Income)
    case _ => None
  }
}
enum Trade:
  case OptionTrade(
    action: PositionType,     // long or short
    optionType: OptionType, // call or put
    expiry: LocalDate,     // Expiration Date
    strike: BigDecimal,     // Strike Price
    premium: BigDecimal,     // Option Premium
    quantity: Int
  ) extends Trade

  case StockTrade(
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
/*
class StrategyContextBuilder {
  private var name: String = ""
  private var underlying: UnderlyingContext = 
}

// Builder for Underlying Context
class UnderlyingBuilder {
  private var symbol: String = ""
  private var price: Double = 0.0
  private var date: LocalDate = LocalDate.now

  def underlying(symbol: String): this.type = { this.symbol = symbol; this }
  def price(value: Double): this.type = { this.price = value; this }
  def on(dateStr: String): UnderlyingContext = {
    this.date = LocalDate.parse(dateStr, java.time.format.DateTimeFormatter.ofPattern("MMMM d, yyyy"))
    UnderlyingContext(symbol, price, date)
  }
}

// Builder for Strategies
class StrategyBuilder(context: StrategyContext) {
  private var trades: List[Trade] = List()
  private var outlook: Option[String] = None
  private var maxRisk: Option[Double] = None
  
  
  

  def sell(optionType: OptionType): OptionTradeBuilder = new OptionTradeBuilder(PositionType.Short, optionType, this)
  def buy(optionType: OptionType): OptionTradeBuilder = new OptionTradeBuilder(PositionType.Long, optionType, this)
  def sell(stock: EquitiesType): EquityTradeBuilder = ???
  def outlook(value: String): this.type = { this.outlook = Some(value); this }
  def maxRisk(value: Double): this.type = { this.maxRisk = Some(value); this }

  def addTrade(trade: Trade): this.type = {
    this.trades = this.trades :+ trade
    this
  }

  def build(): Strategy = Strategy(context, trades)
}

class EquityTradeBuilder(action: PositionType, parent: StrategyBuilder) {
  private var price: Double = 0.0
  private var quantity: Int = 0
  def price(value: Double): StrategyBuilder = {
    parent.addTrade(Trade.StockTrade(action, price,quantity))
  }
}


class OptionTradeBuilder(action: PositionType, optionType: OptionType, parent: StrategyBuilder) {
  private var expiry: String = ""
  private var strike: Double = 0.0
  private var premium: Double = 0.0

  def expiry(value: String): this.type = { this.expiry = value; this }
  def strike(value: Double): this.type = { this.strike = value; this }
  def premium(value: Double): StrategyBuilder = {
    parent.addTrade(Trade.OptionTrade(action, optionType, expiry, strike, premium))
  }
}

// DSL Extension Methods
object DSL {
  def context(body: StrategyContextBuilder => StrategyContext): StrategyContext =
    body(new StrategyContext)

  def strategy(name: String,strategy: String, underlying: UnderlyingContext)(body: StrategyBuilder => Strategy): Strategy =
    body(new StrategyBuilder(context))
}

*/
