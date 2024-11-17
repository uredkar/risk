package com.definerisk.core.OptionStrategies

import java.time.LocalDate

// Underlying asset (e.g., a stock or index)
case class Underlying(symbol: String, price: BigDecimal, date: LocalDate)
given Underlying = Underlying("AAPL", 150.00, LocalDate.now)
// Option type: Call or Put
enum OptionType:
  case Call, Put

// Option contract
case class OptionContract(
  symbol: String,
  underlying: Underlying,
  price: BigDecimal,
  expiryDate: LocalDate,
  strikePrice: BigDecimal,
  optionType: OptionType
)

def coveredCall(
  strikePrice: BigDecimal,
  premium: BigDecimal,
  expiryDate: LocalDate
    )(using underlying: Underlying): OptionsStrategy =
  OptionsStrategy(
    name = "Covered Call",
    legs = Seq(
      StrategyLeg(OptionContract(underlying.symbol, underlying, premium, expiryDate, strikePrice, OptionType.Call), isLong = false)
    )
  )

def straddle(
  strikePrice: BigDecimal,
  callPremium: BigDecimal,
  putPremium: BigDecimal,
  expiryDate: LocalDate
    )(using underlying: Underlying): OptionsStrategy =
  OptionsStrategy(
    name = "Straddle",
    legs = Seq(
      StrategyLeg(OptionContract(underlying.symbol, underlying, callPremium, expiryDate, strikePrice, OptionType.Call), isLong = true),
      StrategyLeg(OptionContract(underlying.symbol, underlying, putPremium, expiryDate, strikePrice, OptionType.Put), isLong = true)
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
    legs = Seq(
      StrategyLeg(OptionContract(underlying.symbol, underlying, buyPremium, expiryDate, lowerStrike, OptionType.Call), isLong = true),
      StrategyLeg(OptionContract(underlying.symbol, underlying, sellPremium, expiryDate, higherStrike, OptionType.Call), isLong = false)
    )
  )

// A single leg of a strategy (long/short option contract)
case class StrategyLeg(contract: OptionContract, isLong: Boolean)

// A complete options strategy
case class OptionsStrategy(name: String, legs: Seq[StrategyLeg])

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

// Example usage
val coveredCallAnalysis = CoveredCallWithAnalysis(coveredCall(155.00, 3.00, LocalDate.now.plusMonths(1)), 3.00, 0.0)
