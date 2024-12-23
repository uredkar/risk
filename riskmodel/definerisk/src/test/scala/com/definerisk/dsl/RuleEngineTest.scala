package com.definerisk.core.models
        


import java.io._
import scala.math.{exp, sqrt, log, Pi}
import java.time.LocalDate
import scala.util.{Try, Success, Failure}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
//import org.scalatest.matchers.in.Matchers
import org.scalatest.BeforeAndAfterAll
import org.scalatest.BeforeAndAfterEach
import java.time.LocalDate
import scala.math.BigDecimal
import java.nio.file.{Files, Paths}
import java.io.{File, FileWriter, PrintWriter}
import scala.compiletime.uninitialized

trait TestPrinterWriter extends PrinterWriter {
  private val stringWriter = new StringWriter()
  val writer: PrintWriter = new PrintWriter(stringWriter)

  override def println(s: String): Unit = writer.println(s)
  def getOutput: String = stringWriter.toString
}

class RuleEngineTest extends AnyFunSuite with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {
    var testPrinterWriter: TestPrinterWriter = uninitialized
    val writer: java.io.PrintWriter = new java.io.PrintWriter(ConfigReader.rulesEngineLogDirectory)
    
}