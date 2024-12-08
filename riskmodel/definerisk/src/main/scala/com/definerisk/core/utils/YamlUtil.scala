package com.definerisk.core.utils.YamlUtil

import io.circe._, io.circe.generic.semiauto._, io.circe.syntax._

//import io.circe.{Decoder, Encoder, Json}
//import io.circe.generic.semiauto._
//import io.circe.syntax._
import io.circe.yaml.parser
import io.circe.yaml.Printer

import java.io.{File, FileWriter, PrintWriter}
import scala.io.Source

import java.time.LocalDate
//import io.circe.java8.time._ // for LocalDate serialization/deserialization


import com.definerisk.core.models.{*,given}

object YamlUtil:
  private val yamlPrinter = Printer.spaces2


  // Encoders and Decoders for Security Subtypes
  given Encoder[OptionType] =  Encoder.instance {
    case _ : OptionType.Call.type => "Call".asJson
    case _ : OptionType.Put.type  => "Put".asJson
  }
  given Decoder[OptionType] = Decoder.decodeString.emap {
    case "Call" => Right(OptionType.Call)
    case "Put"  => Right(OptionType.Put)
    case other  => Left(s"Invalid OptionType: $other")
    }
  //given Decoder[OptionType] = deriveDecoder[OptionType]

  given Encoder[Stock] = deriveEncoder[Stock]
  given Decoder[Stock] = deriveDecoder[Stock]

  given Encoder[Bond] = deriveEncoder[Bond]
  given Decoder[Bond] = deriveDecoder[Bond]

  given Encoder[ETF] = deriveEncoder[ETF]
  given Decoder[ETF] = deriveDecoder[ETF]

  given Encoder[StockOption] = deriveEncoder[StockOption]
  given Decoder[StockOption] = deriveDecoder[StockOption]

  // Encoders and Decoders for Account and Portfolio
  given Encoder[Account] = deriveEncoder[Account]
  given Decoder[Account] = deriveDecoder[Account]

  given Encoder[Portfolio] = deriveEncoder[Portfolio]
  given Decoder[Portfolio] = deriveDecoder[Portfolio]

  // Polymorphic Encoder and Decoder for Security
  given Encoder[Security] = Encoder.instance {
    case stock: Stock =>
      stock.asJson.deepMerge(Json.obj("type" -> Json.fromString("Stock")))
    case bond: Bond =>
      bond.asJson.deepMerge(Json.obj("type" -> Json.fromString("Bond")))
    case etf: ETF =>
      etf.asJson.deepMerge(Json.obj("type" -> Json.fromString("ETF")))
    case stockOption: StockOption =>
      stockOption.asJson.deepMerge(Json.obj("type" -> Json.fromString("StockOption")))
  }

  given Decoder[Security] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "Stock"       => cursor.as[Stock]
      case "Bond"        => cursor.as[Bond]
      case "ETF"         => cursor.as[ETF]
      case "StockOption" => cursor.as[StockOption]
      //case other         => Left(new Exception(s"Unknown security type: $other"))
      //case other         => Left(new DecodingFailure(_,ops))
    }
  }
  // Save Portfolio to a YAML file
  def savePortfolio(portfolio: Portfolio, file: File): Unit =
    val yamlString = yamlPrinter.pretty(portfolio.asJson)
    val writer = new PrintWriter(file)
    try writer.write(yamlString)
    finally writer.close()

  // Load Portfolio from a YAML file
  def loadPortfolio(file: File): Either[io.circe.Error, Portfolio] =
    val yamlContent = Source.fromFile(file).mkString
    parser.parse(yamlContent).flatMap(_.as[Portfolio])

@main def demoYamlUtil() =

  val portfolio = Portfolio(
    accounts = List(
      Account(
        "Account1",
        "BrokerA",
        List(
          Stock("AAPL", 100, 150.0),
          Bond("GOVT", 50, 1000.0, 3.5, 1000.0, LocalDate.of(2030, 12, 31))
        )
      ),
      Account(
        "Account2",
        "BrokerB",
        List(
          ETF("SPY", 20, 400.0),
          StockOption("TSLA", 10, 50.0, OptionType.Call, 750.0, 5.0, LocalDate.of(2024, 6, 30))
        )
      )
    )
  )

  val file = new File("portfolio.yaml")

  // Save Portfolio
  YamlUtil.savePortfolio(portfolio, file)

  // Load Portfolio
  YamlUtil.loadPortfolio(file) match {
    case Right(loadedPortfolio) => println(s"Portfolio loaded: $loadedPortfolio")
    case Left(error)            => println(s"Failed to load portfolio: $error")
  }
