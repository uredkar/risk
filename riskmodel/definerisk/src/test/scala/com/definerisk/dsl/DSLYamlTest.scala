package com.definerisk.dsl
        


import java.io._
import scala.math.{exp, sqrt, log, Pi}
import java.time.LocalDate
import scala.util.{Try, Success, Failure}

import com.definerisk.core.utils.PrettyPrinter.{*, given}
import com.definerisk.core.dsl.{* , given}
import com.definerisk.core.models.{*, given}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate
import scala.math.BigDecimal
import java.nio.file.{Files, Paths}

class DSLYamlTest extends AnyFunSuite with Matchers {
    
   
    test("RunYamlReader") {
        val directory = "./src/main/resources/" // Replace with your directory
        val yamlContents = YamlReader.readYamlFiles(directory)
        yamlContents.foreach { case (file, content) =>
        println(s"File: $file")
        val ctx = DSLProcessor.parse(content) match 
                    case Right(strategy) =>  {
                            printPretty(strategy) 
                            "Ok"
                    }
                    case Left(error) => "Error"
        println(ctx)
        }
    }
    test("Test Long Call Butterfly yaml") {
        import DSLProcessor.*
        val yamlFile = "./src/main/resources/LongCallButterfly.yaml"
        val content = new String(Files.readAllBytes(Paths.get(yamlFile)))
        val ctx = DSLProcessor.parse(content) match 
                    case Right(strategy) =>  {
                            printPretty(strategy) 
                            "Ok"
                    }
                    case Left(error) => "Error"
        println(ctx)
    }

    test("Test All yaml files") {
    
        val directory = "./src/main/resources/" // Replace with your directory
        val yamlContents = YamlReader.readYamlFiles(directory)
        yamlContents.foreach { case (file, content) =>
            println(s"File: $file")
            
            val ctx = DSLProcessor.parse(content) match 
                    case Right(strategy) =>  {
                            printPretty(strategy) 
                            "Ok"
                    }
                    case Left(error) => "Error"
            println(ctx)   
        }
    }

     test("Test All yaml files with Lazy Stream") {
   
       val directory = "./src/main/resources" // Replace with your directory
        
        // Create a processor for Strategy files
        val processor = YamlStreamProcessor[Strategy](directory)

        // Process and print each strategy
        processor.process { strategy =>
            println(s"Lazy Strategy Name: ${strategy.context.name}")
            strategy.legs.map { leg =>
                println(s"Lazy Trades:\n${leg.trades.mkString("\n")}")
            }
        }
    }

    test("Monads") {
        trait MyContainer[F[_]] {
            def get[A](fa: F[A]): A
            }

            given listContainer: MyContainer[List] with {
            def get[A](fa: List[A]): A = fa.head
            }

            def firstElement[F[_], A](container: F[A])(using MyContainer[F]): A = 
                summon[MyContainer[F]].get(container)
            println("\nFirst elemenet -------")
            println(firstElement(List(1, 2, 3)))  
    }

}
