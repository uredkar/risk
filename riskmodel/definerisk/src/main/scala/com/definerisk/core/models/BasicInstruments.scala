package com.definerisk.core.models

import java.time.LocalDate
// Underlying asset (e.g., a stock or index)
case class Underlying(symbol: String, price: BigDecimal, date: LocalDate)


// Option type: Call or Put
enum OptionType:
  case Call, Put

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
    case Income, Vertical, Volatility, Sideways, Leveraged, Synthetic

enum Risk:
    case CappedRisk, UnCappedRisk, CappedReward, UnCappedReward

enum Proficiency:
    case Novice, Intermediate, Advanced, Expert

enum Greeks:
    case Gamma, Theta, Vega, Rho
// Option contract
case class OptionContract(
  symbol: String,
  underlying: Underlying,
  price: BigDecimal,
  expiryDate: LocalDate,
  strikePrice: BigDecimal,
  optionType: OptionType
)
