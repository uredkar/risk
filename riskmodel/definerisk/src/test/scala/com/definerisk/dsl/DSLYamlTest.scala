package com.definerisk.dsl
        


import java.io._
import scala.math.{exp, sqrt, log, Pi}
import java.time.LocalDate


import com.definerisk.core.utils.PrettyPrinter.{*, given}
import com.definerisk.core.dsl.{* , given}
import com.definerisk.core.models.{*, given}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate
import scala.math.BigDecimal
import java.nio.file.{Files, Paths}

class DSLYamlTest extends AnyFunSuite with Matchers {
    
    test("Context Parser") {
        import ContextParser.*
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
        println(s"Context $context")      
    }
    test("Run Yaml Reader") {
        val directory = "./src/main/resources/" // Replace with your directory
        val yamlContents = YamlReader.readYamlFiles(directory)
        yamlContents.foreach { case (file, content) =>
        println(s"File: $file")
        println(s"Content:\n$content")
        println("------")
        val ctx = DSLProcessor.parse(content)
        printPretty(ctx)  
        }
    }
    test("Test Long Call Butterfly yaml") {
        import DSLProcessor.*
        val yamlFile = "./src/main/resources/LongCallButterfly.yaml"
        val yamlContent = new String(Files.readAllBytes(Paths.get(yamlFile)))
        val ctx = parse(yamlContent)
        printPretty(ctx)
    }

    test("Test All yaml files") {
    
        val directory = "./src/main/resources/" // Replace with your directory
        val yamlContents = YamlReader.readYamlFiles(directory)
        yamlContents.foreach { case (file, content) =>
            println(s"File: $file")
            println(s"Content:\n$content")
            println("------")
            val ctx = DSLProcessor.parse(content)
            printPretty(ctx)            
        }
    }
}
