package com.definerisk.core.OptionStrategies

import java.time.LocalDate
import com.definerisk.core.models.{*, given}

given LocalDate = LocalDate.now()

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

