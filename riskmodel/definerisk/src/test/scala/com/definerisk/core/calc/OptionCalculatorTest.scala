package com.definerisk.core.calc
import java.io._
import scala.math.{exp, sqrt, log, Pi}
import java.time.LocalDate

import com.definerisk.core.models.*

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate
import scala.math.BigDecimal

class OptionCalculatorTest extends AnyFunSuite with Matchers {
  given LocalDate = LocalDate.now()
  test("Calculate Option for a mix of option trades and stock trades") {
    
    val expiryDate: LocalDate = summon[LocalDate].plusMonths(3)
    val trades = List(
      Trade.OptionTrade(PositionType.Long, OptionType.Call,expiryDate, 100, 5, 1),
      Trade.OptionTrade(PositionType.Short,OptionType.Put,expiryDate, 90, 4, 1)
    )
    val spotPrices = (80 to 120).map(BigDecimal(_)).toList
    val volatility = 0.2
    OptionsCalculator.generateCsv(spotPrices, trades, volatility, "individualoptions.csv")
    println("CSV generated successfully.") 
  }

  test("Combined Option PnL") {
    val expiryDate: LocalDate = summon[LocalDate].plusMonths(3)
        val trades = List(
        Trade.OptionTrade(PositionType.Long, OptionType.Call,expiryDate, 100, 5, 1),
        Trade.OptionTrade(PositionType.Short,OptionType.Put,expiryDate, 90, 4, 1)
        //Trade.StockTrade(PositionType.Long, 90, 10)
        )
        val strategy = Strategy(
            context = Context(
            name = "Sample Strategy",
            difficulty = "Intermediate",
            direction = "Neutral",
            volatility = "Moderate",
            underlying = None
            ),
            trades = trades
            )

        val priceRange = (80 to 120 by 5).map(BigDecimal(_)).toList

        CombineOptionCalculator.generateCsv(strategy, priceRange, "strategy_pnl.csv")
    }
}
