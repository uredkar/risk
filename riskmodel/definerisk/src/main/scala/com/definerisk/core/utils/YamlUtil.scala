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