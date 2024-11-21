package com.definerisk.core.models

import scala.math.{exp, sqrt, log, Pi}

object OptionTypes:
  enum OptionType:
    case Call, Put

  enum PositionType:
    case Long, Short

  enum Moneyness:
    case ITM, OTM

case class OptionLeg(
    optionType: OptionTypes.OptionType,
    positionType: OptionTypes.PositionType,
    strike: Double,
    premium: Double
)

case class StrategyLeg(name: String, legs: List[OptionLeg])

object GreeksCalculator:
  case class Greeks(delta: Double, gamma: Double, vega: Double, rho: Double)

  def calculate(
      optionType: OptionTypes.OptionType,
      S: Double, // Spot Price
      K: Double, // Strike Price
      T: Double, // Time to Maturity in Years
      r: Double, // Risk-Free Rate
      sigma: Double // Volatility
  ): Greeks =
    val d1 = (log(S / K) + (r + sigma * sigma / 2) * T) / (sigma * sqrt(T))
    val d2 = d1 - sigma * sqrt(T)

    optionType match
      case OptionTypes.OptionType.Call =>
        val delta = cdf(d1)
        val gamma = pdf(d1) / (S * sigma * sqrt(T))
        val vega = S * pdf(d1) * sqrt(T)
        val rho = K * T * exp(-r * T) * cdf(d2)
        Greeks(delta, gamma, vega, rho)

      case OptionTypes.OptionType.Put =>
        val delta = cdf(d1) - 1
        val gamma = pdf(d1) / (S * sigma * sqrt(T))
        val vega = S * pdf(d1) * sqrt(T)
        val rho = -K * T * exp(-r * T) * cdf(-d2)
        Greeks(delta, gamma, vega, rho)

  private def pdf(x: Double): Double =
    1 / sqrt(2 * Pi) * exp(-x * x / 2)

  private def cdf(x: Double): Double =
    0.5 * (1 + erf(x / sqrt(2)))

  private def erf(x: Double): Double =
    // Approximation of the error function
    val t = 1 / (1 + 0.5 * math.abs(x))
    val tau = t * exp(
      -x * x -
        1.26551223 +
        1.00002368 * t +
        0.37409196 * math.pow(t, 2) +
        0.09678418 * math.pow(t, 3) -
        0.18628806 * math.pow(t, 4) +
        0.27886807 * math.pow(t, 5) -
        1.13520398 * math.pow(t, 6) +
        1.48851587 * math.pow(t, 7) -
        0.82215223 * math.pow(t, 8) +
        0.17087277 * math.pow(t, 9)
    )
    if x >= 0 then 1 - tau else tau - 1

trait ProfitLossCalculator:
  def calculatePnL(spotPrices: List[Double]): List[Double]

object Strategies:
  import OptionTypes._

  class Strategy(
      name: String,
      legs: List[OptionLeg]
  ) extends ProfitLossCalculator:
    override def calculatePnL(spotPrices: List[Double]): List[Double] =
      spotPrices.map { S =>
        legs.map {
          case OptionLeg(OptionType.Call, PositionType.Long,  strike, premium) =>
            Math.max(S - strike, 0) - premium
          case OptionLeg(OptionType.Call, PositionType.Short,  strike, premium) =>
            premium - Math.max(S - strike, 0)
          case OptionLeg(OptionType.Put, PositionType.Long,  strike, premium) =>
            Math.max(strike - S, 0) - premium
          case OptionLeg(OptionType.Put, PositionType.Short,  strike, premium) =>
            premium - Math.max(strike - S, 0)
        }.sum
      }

    def maxReward: Double = legs.map(_.premium).sum
    def maxRisk: Double = legs.map(_.premium).map(-_).sum

    def greeks(
        spotPrice: Double,
        r: Double,
        sigma: Double,
        T: Double
    ): GreeksCalculator.Greeks =
      legs.map { leg =>
        GreeksCalculator.calculate(
          leg.optionType,
          spotPrice,
          leg.strike,
          T,
          r,
          sigma
        )
      }.reduce { (g1, g2) =>
        GreeksCalculator.Greeks(
          g1.delta + g2.delta,
          g1.gamma + g2.gamma,
          g1.vega + g2.vega,
          g1.rho + g2.rho
        )
      }

  object StrategyFactory:
    def straddle(strike: Double, premium: Double): Strategy =
      Strategy(
        "Straddle",
        List(
          OptionLeg(OptionType.Call, PositionType.Long, strike, premium),
          OptionLeg(OptionType.Put, PositionType.Long, strike, premium)
        )
      )

    def strangle(callStrike: Double, putStrike: Double, premium: Double): Strategy =
      Strategy(
        "Strangle",
        List(
          OptionLeg(OptionType.Call, PositionType.Long, callStrike, premium),
          OptionLeg(OptionType.Put, PositionType.Long, putStrike, premium)
        )
      )

    // Add more strategies (e.g., Butterfly, Iron Condor) here

@main def testStrategies() =
  import Strategies._
  import OptionTypes._
  val straddle = StrategyFactory.straddle(100, 10)
  val spotPrices = List(80.0, 90.0, 100.0, 110.0, 120.0)
  val pnl = straddle.calculatePnL(spotPrices)
  println(s"Straddle PnL: $pnl")
  println(s"Max Reward: ${straddle.maxReward}")
  println(s"Max Risk: ${straddle.maxRisk}")
