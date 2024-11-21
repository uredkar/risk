package com.definerisk.dsl

import io.circe.yaml.parser
import io.circe.generic.auto._
import io.circe._
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.nio.file.{Files, Paths}
import com.definerisk.dsl.*
//import com.definerisk.core.models.*
import com.definerisk.core.OptionStrategies.*
import com.definerisk.dsl.* 

//import DSL._
case class Context(underlying: Underlying)
case class DSLInput(context: Context, strategy: Strategy)


import io.circe._, io.circe.generic.auto._

// Decoder for LocalDate
given Decoder[LocalDate] = Decoder.decodeString.emap { str =>
  try Right(LocalDate.parse(str, DateTimeFormatter.ISO_DATE))
  catch case _: Exception => Left(s"Invalid date format: $str")
}

// Decoders for enums and case objects
given Decoder[PositionType] = Decoder.decodeString.emap {
  case "Long"  => Right(PositionType.Long)
  case "Short" => Right(PositionType.Short)
  case other   => Left(s"Invalid PositionType: $other")
}

given Decoder[OptionType] = Decoder.decodeString.emap {
  case "Call" => Right(OptionType.Call)
  case "Put"  => Right(OptionType.Put)
  case other  => Left(s"Invalid OptionType: $other")
}

given Decoder[Trade] = Decoder.instance { cursor =>
  cursor.downField("type").as[String].flatMap {
    case "OptionTrade" =>
      for
        action     <- cursor.downField("action").as[PositionType]
        optionType <- cursor.downField("optionType").as[OptionType]
        expiry     <- cursor.downField("expiry").as[String]
        strike     <- cursor.downField("strike").as[Double]
        premium    <- cursor.downField("premium").as[Double]
      yield Trade.OptionTrade(action, optionType, expiry, strike, premium)

    case "StockTrade" =>
      for
        action   <- cursor.downField("action").as[PositionType]
        price    <- cursor.downField("price").as[Double]
        quantity <- cursor.downField("quantity").as[Int]
      yield Trade.StockTrade(action, price, quantity)

    case other =>
      Left(DecodingFailure(s"Unknown trade type: $other", cursor.history))
  }
}

given Decoder[StrategyContext] = Decoder.forProduct5(
  "name",
  "underlying",
  "trades",
  "outlook",
  "maxRisk"
)(StrategyContext.apply)

object DSLProcessor {
  def main(args: Array[String]): Unit = {
    val yamlFile = "./src/main/resources/strategy.yaml"
    val yamlContent = new String(Files.readAllBytes(Paths.get(yamlFile)))

    // Parse YAML into DSLInput
    parser.parse(yamlContent) match {
      case Right(json) =>
        val strategy = json.as[StrategyContext] 
        strategy match {
          case Right(ctx) =>
            println(s"Parsed DSL: $ctx")
          case Left(decodingError) =>
            println(s"Decoding Error: ${decodingError.getMessage}")
        }
      case Left(parsingError) =>
        println(s"Parsing Error: ${parsingError.getMessage}")
    }
    /*
    val underlying = DSL.context {
        _.underlying("ABC").price(28).on("February 19, 2004")
    }
    
    val strategy = DSL.strategy("Bear Call Spread") {
    _.sell(OptionType.Call).expiry("March 2004").strike(30).premium(0.90)
      .buy(OptionType.Call).expiry("April 2004").strike(40).premium(1.20)
      .outlook("Bearish")
      .maxRisk(1000)
      .build()
    }
    
    println(s"Underlying: $underlying")
    println(s"Strategy: $strategy")
    */
  }
}
