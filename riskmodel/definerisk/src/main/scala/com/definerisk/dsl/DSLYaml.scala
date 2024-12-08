package com.definerisk.core.dsl

import com.definerisk.core.models.{*,given}


import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

//import org.yaml.snakeyaml.Yaml
//import org.yaml.snakeyaml.constructor.Constructor



//import io.circe.yaml.parser
//import io.circe.generic.auto._
//import io.circe._
//import io.circe.syntax._
//import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.yaml.syntax._
import io.circe.yaml.Printer
import io.circe._
import io.circe.Decoder.importedDecoder
import scala.io.Source
import scala.util.matching.Regex

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import com.definerisk.core.models.{*, given}



given LocalDate = LocalDate.now()

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

given Decoder[TradeType] = Decoder.decodeString.emap {
  case "Stock" => Right(TradeType.Stock)
  case "ETF"  => Right(TradeType.ETF)
  case other  => Left(s"Invalid TradeType: $other")
}

given Decoder[Underlying] = Decoder.instance { cursor =>
    cursor.downField("underlyingType").as[TradeType].flatMap {
      case TradeType.Stock =>
        for 
          symbol <- cursor.downField("symbol").as[String]
          price  <- cursor.downField("price").as[BigDecimal]
          date   <- cursor.downField("date").as[LocalDate]
        yield StockUnderlying(TradeType.Stock,symbol,price,date)
      case TradeType.ETF =>
        for 
          symbol <- cursor.downField("symbol").as[String]
          price  <- cursor.downField("price").as[BigDecimal]
          date   <- cursor.downField("date").as[LocalDate]
        yield ETFUnderlying(TradeType.ETF,symbol,price,date)
    
      case other =>
        Left(DecodingFailure(s"Unknown underlyingType: $other", cursor.history))        
    }
}

//given Decoder[OptionLeg] = deriveDecoder[OptionLeg]
given Decoder[OptionLeg] with
  def apply(c: HCursor): Decoder.Result[OptionLeg] =
    c.downField("optionLeg").as[HCursor].flatMap { optionLegCursor =>
      for {
        legId  <- optionLegCursor.downField("legId").as[String]
        trades <- optionLegCursor.downField("trades").as[List[Trade]]
      } yield OptionLeg(legId, trades)
    }

given Decoder[Context] = deriveDecoder[Context]
given Decoder[Strategy] = deriveDecoder[Strategy]

given Decoder[Trade] = Decoder.instance { cursor =>
  cursor.downField("type").as[String].flatMap {
    case "OptionTrade" =>
      for
        transactionId     <- cursor.downField("transactionId").as[String]
        transactionDate     <- cursor.downField("transactionDate").as[LocalDate]
        symbol  <- cursor.downField("symbol").as[String]
        action     <- cursor.downField("action").as[PositionType]
        optionType <- cursor.downField("optionType").as[OptionType]
        expiry     <- cursor.downField("expiry").as[LocalDate]
        strike     <- cursor.downField("strike").as[BigDecimal]
        premium    <- cursor.downField("premium").as[BigDecimal]
        quantity   <- cursor.downField("quantity").as[Int]
      yield Trade.OptionTrade("id",LocalDate.now(),symbol,action, optionType, expiry, strike, premium,quantity)

    case "StockTrade" =>
      for
        transactionId     <- cursor.downField("transactionId").as[String]
        transactionDate     <- cursor.downField("transactionDate").as[LocalDate]
        symbol  <- cursor.downField("symbol").as[String]
        action   <- cursor.downField("action").as[PositionType]
        price    <- cursor.downField("price").as[Double]
        quantity <- cursor.downField("quantity").as[Int]
      yield Trade.StockTrade("id",LocalDate.now(),symbol,action, price, quantity)

    case other =>
      Left(DecodingFailure(s"Unknown trade type: $other", cursor.history))
  }
}
/*
given Decoder[Strategy] = Decoder.forProduct3(
  "strategyId",
  "context",
  "legs"
)(Strategy.apply)
*/

class YamlStreamProcessor[T: Decoder](directory: String) {
  private lazy val yamlFiles = LazyList.from { 
    Files
    .list(Paths.get(directory))
    .filter(Files.isRegularFile(_))
    .filter(_.toString.endsWith(".yaml"))
    .iterator()
    .asScala
    .toList
  }
  
  lazy val stream: LazyList[Strategy] = yamlFiles.to(LazyList).flatMap { file =>
    LazyList.from {
      val content = Source.fromFile(file.toFile).mkString
      val parsedResult: Either[String, Strategy] = DSLProcessor.parse(content) // Explicit type
      parsedResult match {
        case Right(strategy) => List(strategy) // Ensure this returns List[Strategy]
        case Left(error) =>
          println(s"Error parsing file ${file.toFile.getName}: $error")
          List.empty[Strategy] // Explicitly return an empty List[Strategy] in case of failure
      }
    }
}

  def process(handler: Strategy => Any): Unit = stream.foreach(handler)
}



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



object DSLProcessor {


  def parse(yamlContent: String): Either[String,Strategy] =
    // Parse YAML into DSLInput
    io.circe.yaml.parser.parse(yamlContent) match {
      case Right(json) =>
        //println(s"json $json")
        json.as[Strategy] match {
          case Right(strategy) => Right(strategy)
          case Left(decodingError) =>
            Left(s"Decoding Error: ${decodingError.getMessage}")
        }
      case Left(parsingError) =>
        Left(s"Parsing Error: ${parsingError.getMessage}")
    
  
  }
}



