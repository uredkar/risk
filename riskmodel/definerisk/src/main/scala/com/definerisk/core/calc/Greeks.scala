package com.definerisk.core.calc

import java.io._
import scala.math.{exp, sqrt, log, Pi}
import java.time.LocalDate
//import breeze.stats.distributions._
import com.definerisk.core.models.*
import java.io.PrintWriter
import java.nio.file.{Files, Paths}

given LocalDate = LocalDate.now()

trait ProfitLossCalculator:
  def calculatePnL(spotPrices: List[BigDecimal]): List[BigDecimal]


extension (s: Strategy) 
  def calculatePnL(spotPrices: List[BigDecimal]): List[BigDecimal] =
    spotPrices.map { S =>
      s.trades.map {
        case Trade.OptionTrade(PositionType.Long,OptionType.Call,_,_,strike, premium,_) =>
          BigDecimal(Math.max(S.toDouble - strike.toDouble, 0)) - premium
        case Trade.OptionTrade( PositionType.Short,OptionType.Call,_,_,strike, premium,_) =>
          (premium - BigDecimal(Math.max(S.toDouble - strike.toDouble, 0)))
        case Trade.OptionTrade( PositionType.Long,OptionType.Put,_,_,strike, premium,_) =>
          BigDecimal(Math.max(strike.toDouble - S.toDouble, 0)) - premium
        case Trade.OptionTrade( PositionType.Short,OptionType.Put,_,_,strike, premium,_) =>
          premium - BigDecimal(Math.max(strike.toDouble - S.toDouble, 0))
        case Trade.StockTrade(action,price, quantity) =>  BigDecimal(-quantity) * price
        //case _ => 0 //println("calculate pnl map case not found")
      }.sum
    }

  //def maxReward: BigDecimal = s.trades.map { case Trade.OptionTrade(PositionType.Long,OptionType.Call,_,strike, premium,_) => premium}.sum
  //def maxRisk: BigDecimal = s.trades.map { case Trade.OptionTrade(PositionType.Long,OptionType.Call,_,strike, premium,_) => -premium}.sum

  def greeks(
    spotPrice: BigDecimal,
    r: BigDecimal,
    sigma: BigDecimal,
    T: BigDecimal
): GreeksCalculator.Greeks =
  s.trades.map {
    case Trade.OptionTrade(PositionType.Long, optionType, _,_,strike, _, _) =>
      GreeksCalculator.calculate(
        optionType,
        spotPrice,
        strike,
        T,
        r,
        sigma
      )
    case Trade.OptionTrade(PositionType.Short, optionType, _,_, strike, _, _) =>
      // Negate Greeks for short positions
      val g = GreeksCalculator.calculate(
        optionType,
        spotPrice,
        strike,
        T,
        r,
        sigma
      )
      g.copy(
        delta = -g.delta,
        gamma = -g.gamma,
        vega = -g.vega,
        rho = -g.rho
      )
    case Trade.StockTrade(action, price, quantity) =>
      // Delta is ±quantity, others are 0
      val delta = action match
        case PositionType.Long  => BigDecimal(quantity)
        case PositionType.Short => BigDecimal(-quantity)

      GreeksCalculator.Greeks(
        delta = delta,
        gamma = BigDecimal(0),
        vega = BigDecimal(0),
        rho = BigDecimal(0)
      )
  }.reduce { (g1, g2) =>
    // Combine Greeks from all trades
    GreeksCalculator.Greeks(
      g1.delta + g2.delta,
      g1.gamma + g2.gamma,
      g1.vega + g2.vega,
      g1.rho + g2.rho
    )
  }

  

object GreeksCalculator:
  case class Greeks(delta: BigDecimal, gamma: BigDecimal, vega: BigDecimal, rho: BigDecimal)

  def calculate(
      optionType: OptionType,
      S: BigDecimal, // Spot Price
      K: BigDecimal, // Strike Price
      T: BigDecimal, // Time to Maturity in Years
      r: BigDecimal, // Risk-Free Rate
      sigma: BigDecimal // Volatility
  ): Greeks =
    val d1 = (log(S.toDouble / K.toDouble) + (r + sigma * sigma / 2) * T) / (sigma * sqrt(T.toDouble))
    val d2 = d1 - sigma * sqrt(T.toDouble)

    optionType match
      case OptionType.Call =>
        val delta = cdf(d1)
        val gamma = pdf(d1) / (S * sigma * sqrt(T.toDouble))
        val vega = S * pdf(d1) * sqrt(T.toDouble)
        val rho = K * T * exp(-r.toDouble * T.toDouble) * cdf(d2)
        Greeks(delta, gamma, vega, rho)

      case OptionType.Put =>
        val delta = cdf(d1) - 1
        val gamma = pdf(d1) / (S * sigma * sqrt(T.toDouble))
        val vega = S * pdf(d1) * sqrt(T.toDouble)
        val rho = -K * T * exp(-r.toDouble * T.toDouble) * cdf(-d2)
        Greeks(delta, gamma, vega, rho)

  private def pdf(x: BigDecimal): BigDecimal =
    1 / sqrt(2 * Pi) * exp(-x.toDouble * x.toDouble / 2)

  private def cdf(x: BigDecimal): BigDecimal =
    0.5 * (1 + erf(x / sqrt(2)))

  private def erf(x: BigDecimal): BigDecimal =
    // Approximation of the error function
    val t = 1 / (1 + 0.5 * math.abs(x.toDouble))
    val tau = t * exp(
      -x.toDouble * x.toDouble -
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



object Strategies:
  
  object StrategyFactory:
    val context = Context(
        name = "Custom Strategy",
        difficulty = "Intermediate",
        direction = "Bullish",
        strategyType = Some(StrategyType.CapitalGain),
        volatility = "Moderate",
        underlying = None
      )
    def straddle(strike: BigDecimal, premium: BigDecimal,  expiryDate: LocalDate): Strategy =
     
      Strategy(
        context,
        List(
          Trade.OptionTrade(PositionType.Long,OptionType.Call,expiryDate, strike, premium,1),
          Trade.OptionTrade(PositionType.Long,OptionType.Put,expiryDate, strike, premium,1)
        )
      )

    def strangle(callStrike: BigDecimal, putStrike: BigDecimal, premium: BigDecimal,expiryDate: LocalDate): Strategy =
      Strategy(
        context,
        List(
          Trade.OptionTrade(PositionType.Long, OptionType.Call,expiryDate, callStrike, premium,1),
          Trade.OptionTrade(PositionType.Long,OptionType.Put, expiryDate, putStrike, premium,1)
        )
      )

    // Add more strategies (e.g., Butterfly, Iron Condor) here
case class PnLPoint(spotPrice: BigDecimal, pnl: BigDecimal)


object CombinedGreekCalculator {
  case class Greeks(
    delta: BigDecimal = 0,
    gamma: BigDecimal = 0,
    vega: BigDecimal = 0,
    rho: BigDecimal = 0,
    theta: BigDecimal = 0
  )

  def calculateGreeks(
      trade: Trade,
      underlyingPrice: BigDecimal,
      timeToExpiry: BigDecimal,
      volatility: BigDecimal,
      riskFreeRate: BigDecimal
  ): Greeks = {
    trade match {
      case Trade.OptionTrade(action, optionType, expiry, _, strike, premium, quantity) =>
        // Black-Scholes Greeks for options
        val d1 = (Math.log((underlyingPrice / strike).toDouble) +
          ((riskFreeRate + (volatility * volatility) / 2) * timeToExpiry.toDouble)) /
          (volatility.toDouble * Math.sqrt(timeToExpiry.toDouble))
        val d2 = d1 - volatility.toDouble * Math.sqrt(timeToExpiry.toDouble)

        //val normalD1 = breeze.stats.distributions.Gaussian(0, 1).cdf(d1)
        //val normalD2 = breeze.stats.distributions.Gaussian(0, 1).cdf(d2)
        val normalD1 = normCDF(d1.toDouble)
        val normalD2 = normCDF(d2.toDouble)

        val delta = optionType match {
          case OptionType.Call => normalD1
          case OptionType.Put  => normalD1 - 1
        }

        val gamma = Math.exp(-d1.toDouble * d1.toDouble / 2) / (underlyingPrice.toDouble * volatility.toDouble * Math.sqrt(2 * Math.PI * timeToExpiry.toDouble))

        val vega = underlyingPrice.toDouble * Math.exp(-d1.toDouble * d1.toDouble / 2) * Math.sqrt(timeToExpiry.toDouble) / Math.sqrt(2 * Math.PI)

        val rho = optionType match {
          case OptionType.Call => normalD2 * strike.toDouble * Math.exp(-riskFreeRate.toDouble * timeToExpiry.toDouble)
          case OptionType.Put  => -normalD2 * strike.toDouble * Math.exp(-riskFreeRate.toDouble * timeToExpiry.toDouble)
        }

        val theta = optionType match {
          case OptionType.Call =>
            (-underlyingPrice.toDouble * Math.exp(-d1.toDouble * d1.toDouble / 2) * volatility.toDouble / (2 * Math.sqrt(2 * Math.PI * timeToExpiry.toDouble))) -
              riskFreeRate.toDouble * strike.toDouble * Math.exp(-riskFreeRate.toDouble * timeToExpiry.toDouble) * normalD2
          case OptionType.Put =>
            (-underlyingPrice.toDouble * Math.exp(-d1.toDouble * d1.toDouble / 2) * volatility.toDouble / (2 * Math.sqrt(2 * Math.PI * timeToExpiry.toDouble))) +
              riskFreeRate.toDouble * strike.toDouble * Math.exp(-riskFreeRate.toDouble * timeToExpiry.toDouble) * normalD2
        }

        val multiplier = if (action == PositionType.Long) 1 else -1
        Greeks(
          delta = BigDecimal(delta) * multiplier * quantity,
          gamma = BigDecimal(gamma) * multiplier * quantity,
          vega = BigDecimal(vega) * multiplier * quantity,
          rho = BigDecimal(rho) * multiplier * quantity,
          theta = BigDecimal(theta) * multiplier * quantity
        )

      case _: Trade.StockTrade | _: Trade.ETFTrade =>
        // Non-option trades do not contribute to Greeks in this implementation
        Greeks()
    }
  }

  def calculateStrategyGreeks(
      trades: List[Trade],
      underlyingPrice: BigDecimal,
      timeToExpiry: BigDecimal,
      volatility: BigDecimal,
      riskFreeRate: BigDecimal
  ): Greeks = {
    trades.foldLeft(Greeks()) { (totalGreeks, trade) =>
      val tradeGreeks = calculateGreeks(trade, underlyingPrice, timeToExpiry, volatility, riskFreeRate)
      Greeks(
        delta = totalGreeks.delta + tradeGreeks.delta,
        gamma = totalGreeks.gamma + tradeGreeks.gamma,
        vega = totalGreeks.vega + tradeGreeks.vega,
        rho = totalGreeks.rho + tradeGreeks.rho,
        theta = totalGreeks.theta + tradeGreeks.theta
      )
    }
  }
  def generateCsvWithGreeks(
      strategy: Strategy,
      underlyingPrices: List[BigDecimal],
      volatility: BigDecimal,
      riskFreeRate: BigDecimal,
      filePath: String
  ): Unit = {
    val csvContent = new StringBuilder
    csvContent.append("SpotPrice,PnL,Delta,Gamma,Vega,Rho,Theta\n")

    underlyingPrices.foreach { price =>
      val combinedPnL = CombineOptionCalculator.calculateCombinedPnL(strategy.trades, List(price)).head
      val combinedGreeks = calculateStrategyGreeks(
        strategy.trades,
        underlyingPrice = price,
        timeToExpiry = BigDecimal(0.25), // Example: 3 months
        volatility = volatility,
        riskFreeRate = riskFreeRate
      )
      csvContent.append(
        s"$price,$combinedPnL,${combinedGreeks.delta},${combinedGreeks.gamma},${combinedGreeks.vega},${combinedGreeks.rho},${combinedGreeks.theta}\n"
      )
    }

    Files.write(Paths.get(filePath), csvContent.toString.getBytes)
    println(s"CSV with Greeks and P&L generated at: $filePath")
  }
}

@main def testStrategies() =
  import Strategies._
  //import OptionTypes._
  val expiryDate = LocalDate.of(2004,1,1)
  val straddle = StrategyFactory.straddle(100, 10, expiryDate)
  val spotPrices = List(BigDecimal(80.0), BigDecimal(90.0), BigDecimal(100.0), BigDecimal(110.0), BigDecimal(120.0))
  val pnl = straddle.calculatePnL(spotPrices)
  println(s"Straddle PnL: $pnl")
  //println(s"Max Reward: ${straddle.maxReward}")
  //println(s"Max Risk: ${straddle.maxRisk}")
  
  //val pnlValues = spotPrices.map(spot => PnLPoint(spot, spot - 100)) // Example PnL
  val file = new File("pnl_data.csv")
  val writer = new BufferedWriter(new FileWriter(file))
  writer.write("SpotPrice,PnL\n")
  val points = spotPrices zip pnl
  points.foreach { point =>
    writer.write(s"${point(0)},${point(1)}\n")
  }
  writer.close()
