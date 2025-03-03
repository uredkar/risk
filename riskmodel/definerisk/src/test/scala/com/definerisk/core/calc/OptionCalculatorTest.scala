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
      Trade.OptionTrade("id",LocalDate.now(),"AAPL000",PositionType.Long, OptionType.Call,expiryDate, 100, 5, 1),
      Trade.OptionTrade("id",LocalDate.now(),"AAPL000",PositionType.Short,OptionType.Put,expiryDate, 90, 4, 1)
    )
    val spotPrices = (80 to 120).map(BigDecimal(_)).toList
    val volatility = 0.2
    OptionsCalculator.generateCsv(spotPrices, trades, volatility, "individualoptions.csv")
    println("CSV generated successfully.") 
  }

  test("Combined Option PnL") {
    val expiryDate: LocalDate = summon[LocalDate].plusMonths(3)
    val trades = List(
    Trade.OptionTrade("id",LocalDate.now(),"AAPL000",PositionType.Long, OptionType.Call,expiryDate, 100, 5, 1),
    Trade.OptionTrade("id",LocalDate.now(),"AAPL000",PositionType.Short,OptionType.Put,expiryDate, 90, 4, 1)
    //Trade.StockTrade(PositionType.Long, 90, 10)
    )
    val strategy = Strategy(
        strategyId = "id",
        context = Context(
        name = "Sample Strategy",
        difficulty = "Intermediate",
        direction = "Neutral",
        volatility = "Moderate",
        underlying = None
        ),
        legs = List(OptionLeg("id",trades))
        )

    val priceRange = (80 to 120 by 5).map(BigDecimal(_)).toList

    CombineOptionCalculator.generateCsv(strategy, priceRange, "strategy_pnl.csv")
    }

    test("Combined Greeks"){
        val expiryDate: LocalDate = summon[LocalDate].plusMonths(3)
        val trades = List(
            Trade.OptionTrade("id",LocalDate.now(),"AAPL000",PositionType.Long, OptionType.Call,expiryDate, 100, 5, 1),
            Trade.OptionTrade("id",LocalDate.now(),"AAPL000",PositionType.Short,OptionType.Put,expiryDate, 90, 4, 1)
        )
        val strategy = Strategy(
            strategyId = "id",
            context = Context(
            name = "Sample Strategy",
            difficulty = "Intermediate",
            direction = "Neutral",
            volatility = "Moderate",
            underlying = None
            ),
            legs = List(OptionLeg("id",trades))
        )
        val priceRange = (80 to 120 by 5).map(BigDecimal(_)).toList
        CombinedGreekCalculator.generateCsvWithGreeks(
            strategy = strategy,
            underlyingPrices = priceRange,
            volatility = BigDecimal(0.2),    // Example: 20% volatility
            riskFreeRate = BigDecimal(0.05), // Example: 5% risk-free rate
            filePath = "strategy_pnl_and_greeks.csv"
        )
    }

     test("Covered Call with Downside Protection for fun"){
        val expiryDate: LocalDate = summon[LocalDate].plusMonths(3)
        val trades = List(
            Trade.StockTrade("id",LocalDate.now(),"AAPL",PositionType.Long, 138,  100),
            Trade.OptionTrade("id",LocalDate.now(),"AAPL000",PositionType.Long,OptionType.Put,expiryDate, 130, 9.36, 1)
        )
        val strategy = Strategy(
            strategyId = "id",
            context = Context(
            name = "Sample Strategy",
            difficulty = "Intermediate",
            direction = "Neutral",
            volatility = "Moderate",
            underlying = None
            ),
            legs = List(OptionLeg("id",trades))
        )
        val priceRange = (100 to 200 by 5).map(BigDecimal(_)).toList
        CombinedGreekCalculator.generateCsvWithGreeks(
            strategy = strategy,
            underlyingPrices = priceRange,
            volatility = BigDecimal(0.2),    // Example: 20% volatility
            riskFreeRate = BigDecimal(0.05), // Example: 5% risk-free rate
            filePath = "coveredcall.csv"
        )
    }
}
