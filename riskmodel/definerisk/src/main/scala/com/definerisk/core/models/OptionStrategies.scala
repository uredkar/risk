package com.definerisk.core.OptionStrategies

import java.time.LocalDate
import com.definerisk.core.models.{*, given}
/*
// A single leg of a strategy (long/short option contract)
case class StrategyLeg(contract: Trade.OptionTrade, isLong: Boolean)

// A complete options strategy
//case class OptionsStrategy(name: String, outlook: String, maxRisk: Double,legs: List[])
//case class Underlying(symbol: String, price: Double, date: String)
//case class OptionTrade(action: String, `type`: String, expiry: String, strike: Double, premium: Double)
case class Strategy(name: String, outlook: String, maxRisk: Double, trades: List[Trade.OptionTrade])
enum PositionType:
  case Long, Short

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

*/

  
def coveredCall(
  strikePrice: BigDecimal,
  premium: BigDecimal,
  expiryDate: LocalDate
    )(using underlying: Underlying): Strategy =

  val context = Context(
    name = "Custom Strategy",
    difficulty = "Intermediate",
    direction = "Bullish",
    strategyType = Some(StrategyType.CapitalGain),
    volatility = "Moderate",
    underlying = Some(underlying)
  )        
   
  Strategy(
    context,
    trades = List(
      Trade.OptionTrade(PositionType.Long,OptionType.Call,expiryDate, strikePrice, premium,1)
    )
  )

def straddle(
  strikePrice: BigDecimal,
  callPremium: BigDecimal,
  putPremium: BigDecimal,
  expiryDate: LocalDate,
  isLong: Boolean
    )(using underlying: Underlying): Strategy =
  val context = Context(
    name = "Custom Strategy",
    difficulty = "Intermediate",
    direction = "Bullish",
    strategyType = Some(StrategyType.CapitalGain),
    volatility = "Moderate",
    underlying = Some(underlying)
  )        
  Strategy(
    context,
    trades = List(Trade.OptionTrade(PositionType.Long,OptionType.Call,expiryDate, strikePrice, callPremium,1),
                Trade.OptionTrade(PositionType.Long,OptionType.Put,expiryDate, strikePrice, putPremium,1))
  )

def bullCallSpread(
  lowerStrike: BigDecimal,
  higherStrike: BigDecimal,
  buyPremium: BigDecimal,
  sellPremium: BigDecimal,
  expiryDate: LocalDate
)(using underlying: Underlying): Strategy =
  val context = Context(
    name = "Custom Strategy",
    difficulty = "Intermediate",
    direction = "Bullish",
    strategyType = Some(StrategyType.CapitalGain),
    volatility = "Moderate",
    underlying = Some(underlying)
  )  
  Strategy(
    context,
    trades = List(Trade.OptionTrade(PositionType.Long,OptionType.Call,expiryDate, lowerStrike, buyPremium,1),
                  Trade.OptionTrade(PositionType.Short,OptionType.Put,expiryDate, higherStrike, sellPremium,1))
  )
  


trait IncomeStrategy:
  def maxIncome: BigDecimal

trait VolatilityStrategy:
  def maxRisk: BigDecimal

// Combine traits with an intersection type
type IncomeAndVolatility = IncomeStrategy & VolatilityStrategy

// Example of an intersection type
case class CoveredCallWithAnalysis(
  strategy: Strategy,
  income: BigDecimal,
  risk: BigDecimal
) extends IncomeStrategy, VolatilityStrategy:
  def maxIncome: BigDecimal = income
  def maxRisk: BigDecimal = risk

