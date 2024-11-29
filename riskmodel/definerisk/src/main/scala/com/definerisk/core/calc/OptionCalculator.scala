package com.definerisk.core.calc

import java.io.PrintWriter
import scala.math._

import com.definerisk.core.models.*

def erf(x: Double): Double = {
  // Constants for approximation
  val a1 = 0.254829592
  val a2 = -0.284496736
  val a3 = 1.421413741
  val a4 = -1.453152027
  val a5 = 1.061405429
  val p = 0.3275911

  // Abramowitz and Stegun formula 7.1.26
  val sign = if (x < 0) -1 else 1
  val absX = math.abs(x)
  val t = 1.0 / (1.0 + p * absX)
  val y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * math.exp(-absX * absX)

  sign * y
}

def normCDF(x: Double): Double = {
  0.5 * (1 + erf(x / sqrt(2)))
}


object OptionsCalculator {
    

  val riskFreeRate = 0.05 // Annual risk-free rate

  def blackScholes(
      spotPrice: BigDecimal,
      strike: BigDecimal,
      timeToExpiry: BigDecimal,
      riskFreeRate: BigDecimal,
      volatility: BigDecimal,
      optionType: OptionType
  ): (BigDecimal, TradeGreeks) = {
    val d1 = (log(spotPrice.toDouble / strike.toDouble) + (riskFreeRate + 0.5 * pow(volatility.toDouble, 2)) * timeToExpiry) /
      (volatility * sqrt(timeToExpiry.toDouble))
    val d2 = d1 - volatility * sqrt(timeToExpiry.toDouble)

    val nd1 = 0.5 * (1 + erf(d1.toDouble / sqrt(2)))
    val nd2 = 0.5 * (1 + erf(d2.toDouble / sqrt(2)))

    val price = optionType match {
      case OptionType.Call  => spotPrice * nd1 - strike * exp(-riskFreeRate.toDouble * timeToExpiry.toDouble) * nd2
      case OptionType.Put => strike * exp(-riskFreeRate.toDouble * timeToExpiry.toDouble) * (1 - nd2) - spotPrice * (1 - nd1)
    }

    val delta = optionType match {
      case OptionType.Call => nd1
      case OptionType.Put => nd1 - 1
    }

    val gamma = exp(-0.5 * pow(d1.toDouble, 2)) / (spotPrice * volatility * sqrt(2 * Pi * timeToExpiry.toDouble))
    val vega = spotPrice * sqrt(timeToExpiry.toDouble) * exp(-0.5 * pow(d1.toDouble, 2)) / sqrt(2 * Pi)
    val rho = optionType match {
      case OptionType.Call => strike * timeToExpiry * exp(-riskFreeRate.toDouble * timeToExpiry.toDouble) * nd2
      case OptionType.Put => -strike * timeToExpiry * exp(-riskFreeRate.toDouble * timeToExpiry.toDouble) * (1 - nd2)
    }
    val theta = optionType match {
      case OptionType.Call =>
        -(spotPrice * exp(-0.5 * pow(d1.toDouble, 2)) * volatility) / (2 * sqrt(2 * Pi * timeToExpiry.toDouble)) -
          riskFreeRate * strike * exp(-riskFreeRate.toDouble * timeToExpiry.toDouble) * nd2
      case OptionType.Put =>
        -(spotPrice * exp(-0.5 * pow(d1.toDouble, 2)) * volatility) / (2 * sqrt(2 * Pi * timeToExpiry.toDouble)) +
          riskFreeRate * strike * exp(-riskFreeRate.toDouble * timeToExpiry.toDouble) * (1 - nd2)
    }

    (price, TradeGreeks(delta, gamma, vega, rho, theta))
  }

  def generateCsv(
      spotPrices: List[BigDecimal],
      trades: List[Trade.OptionTrade],
      volatility: BigDecimal,
      fileName: String
  ): Unit = {
    val writer = new PrintWriter(fileName)
    writer.println("SpotPrice,TradeType,Strike,Premium,Quantity,Price,Delta,Gamma,Vega,Rho,Theta,PnL")

    spotPrices.foreach { spot =>
      trades.foreach { trade =>
        val (price, greeks) = blackScholes(
          spot,
          trade.strike,
          trade.timeToExpiry,
          riskFreeRate,
          volatility,
          trade.optionType
        )
        val pnl = trade.quantity * (price - trade.premium) * (if (trade.action == PositionType.Long) 1 else -1)
        writer.println(
          s"$spot,${trade.optionType},${trade.strike},${trade.premium},${trade.quantity},$price," +
            s"${greeks.delta},${greeks.gamma},${greeks.vega},${greeks.rho},${greeks.theta},$pnl"
        )
      }
    }
    writer.close()
  }
}

object CombineOptionCalculator {
    def calculateCombinedPnL(
        trades: List[Trade],
        underlyingPrices: List[BigDecimal]
    ): List[BigDecimal] = {
        underlyingPrices.map { price =>
            trades.foldLeft(BigDecimal(0)) { (totalPnL, trade) =>
            trade match {
                case Trade.OptionTrade(action, optionType, expiry, _, strike, premium, quantity) =>
                val intrinsicValue = optionType match {
                    case OptionType.Call => (price - strike).max(0)
                    case OptionType.Put  => (strike - price).max(0)
                }
                val tradePnL = action match {
                    case PositionType.Long =>
                    (intrinsicValue - premium) * quantity
                    case PositionType.Short =>
                    (premium - intrinsicValue) * quantity
                }
                totalPnL + tradePnL

                case Trade.StockTrade(action, tradePrice, quantity) =>
                val tradePnL = action match {
                    case PositionType.Long =>
                    (price - tradePrice) * quantity
                    case PositionType.Short =>
                    (tradePrice - price) * quantity
                }
                totalPnL + tradePnL

                case Trade.ETFTrade(action, tradePrice, quantity) =>
                val tradePnL = action match {
                    case PositionType.Long =>
                    (price - tradePrice) * quantity
                    case PositionType.Short =>
                    (tradePrice - price) * quantity
                }
                totalPnL + tradePnL
            }
            }
        }
    }

    def generateCsv(
        strategy: Strategy,
        underlyingPrices: List[BigDecimal],
        fileName: String
    ): Unit = {
        val combinedPnL = calculateCombinedPnL(strategy.trades, underlyingPrices)
        val writer = new PrintWriter(fileName)
        writer.println("SpotPrice,PnL")

        underlyingPrices.zip(combinedPnL).foreach { case (price, pnl) =>
            writer.println(s"$price,$pnl")
        }
        writer.close()
        //Files.write(Paths.get(filePath), csvContent.toString.getBytes)
        println(s"CSV generated at: $fileName")
    }
}