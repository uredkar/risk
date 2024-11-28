package com.definerisk.core.calc

import java.io._
import scala.math.{exp, sqrt, log, Pi}
import java.time.LocalDate

import com.definerisk.core.models.*

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate
import scala.math.BigDecimal

class GreeksCalculatorTest extends AnyFunSuite with Matchers {
  import Strategies.*
  test("Calculate Greeks for a mix of option trades and stock trades") {
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
    
  }
}
