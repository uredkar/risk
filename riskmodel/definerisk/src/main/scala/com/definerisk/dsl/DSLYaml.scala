package com.definerisk.dsl
import com.definerisk.core.utils.PrettyPrinter.{*, given}
import io.circe.yaml.parser
import io.circe.generic.auto._
import io.circe._
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import scala.io.Source

import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.yaml.Printer

import scala.util.matching.Regex

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.nio.file.{Files, Paths}
import com.definerisk.dsl.*
//import com.definerisk.core.models.*
//import com.definerisk.core.OptionStrategies.*
import com.definerisk.dsl.* 

//import DSL._
///case class Context(underlying: Underlying)
///case class DSLInput(context: Context, strategy: Strategy)


import io.circe._, io.circe.generic.auto._

// Decoder for LocalDate
given Decoder[LocalDate] = Decoder.decodeString.emap { str =>
  try Right(LocalDate.parse(str, DateTimeFormatter.ISO_DATE))
  catch case _: Exception => Left(s"Invalid date format: $str")
}

// Decoders for enums and case objects
given Decoder[StrategyType] = Decoder.decodeString.emap {
  case "CapitalGain"  => Right(StrategyType.CapitalGain)
  case "Income" => Right(StrategyType.Income)
  case other   => Left(s"Invalid StrategyType: $other")
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
        expiry     <- cursor.downField("expiry").as[LocalDate]
        strike     <- cursor.downField("strike").as[BigDecimal]
        premium    <- cursor.downField("premium").as[BigDecimal]
        quantity   <- cursor.downField("quantity").as[Int]
      yield Trade.OptionTrade(action, optionType, expiry, strike, premium,quantity)

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

given Decoder[Strategy] = Decoder.forProduct2(
  "context",
  "trades"
)(Strategy.apply)



given PrettyPrinter[Strategy] with
  def prettyPrint(strategy: Strategy): String =
    s"name ${strategy.context.name} difficulty ${strategy.context.difficulty} \n" +
    s"underlying ${strategy.context.underlying} outlook ${strategy.context.outlook} \n" +
    s"strategyType ${strategy.context.strategyType}\n" +
    s"maxRisk ${strategy.context.maxRisk}\n" +
    s"trades ${strategy.trades}\n"

object YamlReader:
  def readYamlFiles(directory: String): Map[String, String] =
    // Find all .yaml files in the directory
    val yamlFiles = Files.list(Paths.get(directory))
      .iterator()
      .asScala
      .filter(path => path.toString.endsWith(".yaml"))
      .toList

    // Read the content of each file and create a map
    yamlFiles.map { path =>
      val fileName = path.getFileName.toString
      val content = Source.fromFile(path.toFile).mkString
      fileName -> content
    }.toMap

@main def runYamlReader(): Unit =
  val directory = "./src/main/resources/" // Replace with your directory
  val yamlContents = YamlReader.readYamlFiles(directory)
  yamlContents.foreach { case (file, content) =>
    println(s"File: $file")
    println(s"Content:\n$content")
    println("------")
    val ctx = DSLProcessor.parse(content)
    printPretty(ctx)            

  }

object DSLProcessor {
  def main(args: Array[String]): Unit = 
    val yamlFile = "./src/main/resources/LongCallButterfly.yaml"
    val yamlContent = new String(Files.readAllBytes(Paths.get(yamlFile)))
    val ctx = parse(yamlContent)
    printPretty(ctx)

  def parse(yamlContent: String): Strategy =
    // Parse YAML into DSLInput
    io.circe.yaml.parser.parse(yamlContent) match {
      case Right(json) =>
        println(s"json $json")
        json.as[Strategy] match {
          case Right(strategy) => strategy
          case Left(decodingError) =>
            throw new IllegalArgumentException(s"Decoding Error: ${decodingError.getMessage}")
        }
      case Left(parsingError) =>
        throw new IllegalArgumentException(s"Parsing Error: ${parsingError.getMessage}")
    
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


object StrategyParser:

  private val symbolRegex: Regex = raw"(\w+) is trading at \$$(\d+\.\d+) on (\w+ \d{1,2}, \d{4})\.".r
  private val optionTradeRegex: Regex = raw"Buy the (\w+) (\d{4}) \$$(\d+\.\d+) strike (\w+) for \$$(\d+\.\d+)\.".r

  def parseUnderlying(input: String): Underlying =
    input match
      case symbolRegex(symbol, price, date) =>
        val parsedDate = LocalDate.parse(date, DateTimeFormatter.ofPattern("MMMM d, yyyy"))
        Underlying(symbol, BigDecimal(price), parsedDate)
      case _ =>
        throw new IllegalArgumentException(s"Invalid format for underlying information: $input")

  def parseTrades(input: String): List[Trade] =
    input match
      case optionTradeRegex(month, year, strike, optionType, premium) =>
        val expiry = LocalDate.parse(s"$month 1 $year", DateTimeFormatter.ofPattern("MMMM d yyyy")).withDayOfMonth(1).plusMonths(1).minusDays(1)
        List(
          Trade.OptionTrade(
            action = PositionType.Long,
            optionType = if optionType.equalsIgnoreCase("call") then OptionType.Call else OptionType.Put,
            expiry = expiry,
            strike = BigDecimal(strike),
            premium = BigDecimal(premium),
            quantity = 1
          )
        )
      case _ =>
        throw new IllegalArgumentException(s"Invalid format for trade information: $input")

  def parse(input: String): Strategy =
    // Sample hardcoded parsing; expand for full inputs
    val underlyingInput = "ABCD is trading at $28.88 on February 19, 2004."
    val tradesInput = "Buy the January 2005 $27.50 strike call for $4.38."

    val underlying = parseUnderlying(underlyingInput)
    val trades = parseTrades(tradesInput)

    val context = Context(
      name = "Custom Strategy",
      difficulty = "Intermediate",
      direction = "Bullish",
      strategyType = Some(StrategyType.CapitalGain),
      volatility = "Moderate",
      underlying = Some(underlying)
    )

    Strategy(context, trades)

@main def runParser() =
  val strategy = StrategyParser.parse("")
  println(strategy)

object ContextParser:

  // Regular expressions to extract context fields
  private val nameRegex: Regex = raw"""name:\s*"([^"]+)"""".r
  private val difficultyRegex: Regex = raw"""difficulty:\s*"([^"]+)"""".r
  private val directionRegex: Regex = raw"""direction:\s*"([^"]+)"""".r
  private val strategyTypeRegex: Regex = raw"""strategyType:\s*"([^"]+)"""".r
  private val volatilityRegex: Regex = raw"""volatility:\s*"([^"]+)"""".r
  
  private val multilineUnderlyingRegex: Regex =
    raw"""underlying:\s*\{\s*symbol:\s*"([^"]+)"\s*,?\s*price:\s*([\d.]+)\s*,?\s*date:\s*"([^"]+)"\s*\}""".r

  // Helper function to extract matches
  private def extract(regex: Regex, input: String): Option[String] =
    regex.findFirstMatchIn(input).map(_.group(1))

  def parseContext(input: String): Context =
    val name = extract(nameRegex, input).getOrElse(throw new IllegalArgumentException("Missing context name"))
    val difficulty = extract(difficultyRegex, input).getOrElse(throw new IllegalArgumentException("Missing context difficulty"))
    val direction = extract(directionRegex, input).getOrElse(throw new IllegalArgumentException("Missing context direction"))
    val strategyType = extract(strategyTypeRegex, input).getOrElse(throw new IllegalArgumentException("Missing context strategyType"))
    val volatility = extract(volatilityRegex, input).getOrElse(throw new IllegalArgumentException("Missing context volatility"))

   
    val underlying = multilineUnderlyingRegex.findFirstMatchIn(input).map {
      case multilineUnderlyingRegex(symbol, price, date) =>
        println(s"Matched underlying: symbol=$symbol, price=$price, date=$date") // Debugging: Show match
        val parsedDate = LocalDate.parse(date, DateTimeFormatter.ofPattern("yyyy-MM-dd"))
        Underlying(symbol, BigDecimal(price), parsedDate)
      case _ =>
        throw new IllegalArgumentException("Invalid format for underlying information")
    }
    Context(
      name = name,
      difficulty = difficulty,
      direction = direction,
      strategyType =  StrategyType.fromString(strategyType),
      volatility = volatility,
      underlying = underlying
    )

@main def runContextParser() =
  val input = """
    name: "Long Call Butterfly"
    difficulty: "Intermediate"
    dir5
    direction: "Neutral"
    strategyType: "Capital Gain"
    volatility: "Low"
    underlying: { symbol: "ABC",price: 50.0, date: "2004-05-17" }
  """

  val context = ContextParser.parseContext(input)
  println(context)  