package com.definerisk.core.OptionStrategies

import java.time.LocalDate
import com.definerisk.core.models.*

// A single leg of a strategy (long/short option contract)
case class StrategyLeg(contract: OptionContract, isLong: Boolean)

// A complete options strategy
case class OptionsStrategy(name: String, outlook: String, maxRisk: Double,legs: Seq[StrategyLeg])
//case class Underlying(symbol: String, price: Double, date: String)
case class OptionTrade(action: String, `type`: String, expiry: String, strike: Double, premium: Double)
case class Strategy(name: String, outlook: String, maxRisk: Double, trades: List[OptionTrade])

def coveredCall(
  strikePrice: BigDecimal,
  premium: BigDecimal,
  expiryDate: LocalDate
    )(using underlying: Underlying): OptionsStrategy =
  OptionsStrategy(
    name = "Covered Call",
    outlook = "Bulish",
    maxRisk = 10,
    legs = Seq(
      StrategyLeg(OptionContract(underlying.symbol, underlying, premium, expiryDate, strikePrice, OptionType.Call), isLong = false)
    )
  )

def straddle(
  strikePrice: BigDecimal,
  callPremium: BigDecimal,
  putPremium: BigDecimal,
  expiryDate: LocalDate,
  isLong: Boolean
    )(using underlying: Underlying): OptionsStrategy =
  OptionsStrategy(
    name = "Straddle",
    outlook = "Bulish",
    maxRisk = 10,
    legs = Seq(
      StrategyLeg(OptionContract(underlying.symbol, underlying, callPremium, expiryDate, strikePrice, OptionType.Call), isLong = isLong),
      StrategyLeg(OptionContract(underlying.symbol, underlying, putPremium, expiryDate, strikePrice, OptionType.Put), isLong = isLong)
    )
  )

def bullCallSpread(
  lowerStrike: BigDecimal,
  higherStrike: BigDecimal,
  buyPremium: BigDecimal,
  sellPremium: BigDecimal,
  expiryDate: LocalDate
)(using underlying: Underlying): OptionsStrategy =
  OptionsStrategy(
    name = "Bull Call Spread",
    outlook = "Bulish",
    maxRisk = 10,
    legs = Seq(
      StrategyLeg(OptionContract(underlying.symbol, underlying, buyPremium, expiryDate, lowerStrike, OptionType.Call), isLong = true),
      StrategyLeg(OptionContract(underlying.symbol, underlying, sellPremium, expiryDate, higherStrike, OptionType.Call), isLong = false)
    )
  )


trait IncomeStrategy:
  def maxIncome: BigDecimal

trait VolatilityStrategy:
  def maxRisk: BigDecimal

// Combine traits with an intersection type
type IncomeAndVolatility = IncomeStrategy & VolatilityStrategy

// Example of an intersection type
case class CoveredCallWithAnalysis(
  strategy: OptionsStrategy,
  income: BigDecimal,
  risk: BigDecimal
) extends IncomeStrategy, VolatilityStrategy:
  def maxIncome: BigDecimal = income
  def maxRisk: BigDecimal = risk

