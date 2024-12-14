package com.definerisk.core.models
        


import java.io._
import scala.math.{exp, sqrt, log, Pi}
import java.time.LocalDate
import scala.util.{Try, Success, Failure}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import java.time.LocalDate
import scala.math.BigDecimal
import java.nio.file.{Files, Paths}
import java.io.{File, FileWriter, PrintWriter}

class RuleEngineTest extends AnyFunSuite with Matchers with BeforeAndAfterAll {
    val writer: java.io.PrintWriter = new java.io.PrintWriter(ConfigReader.rulesEngineLogDirectory)
    given PrinterWriter with
        def println(s: String): Unit = writer.println(s)
    
    override def afterAll(): Unit = {
        writer.close()
        super.afterAll()
    }        
    def testForwardChaining(rules: List[Rule])(using writer: PrinterWriter) = {

        val facts = Set(
            Compound("male", List(Atom("john"))),
            Compound("female", List(Atom("susan"))),
            Compound("parent", List(Atom("john"), Atom("susan")))
        )
        
        writer.println("-----------------------------------------------------------------")
        writer.println(s"testForwardChaining")
        writer.println("-----------------------------------------------------------------")

        //val derivedFactsNot = forwardChainingWithNot(facts, rules)
        val derivedFacts = forwardChaining(facts, rules)
        writer.println(s"\t\t*********************derivedFacts $derivedFacts")
        {
            writer.println("----------------------ancestor-------------------------------------------")
            val query = Compound("ancestor", List(Atom("john"), Atom("susan")))
            val result1 = validateQuery(query, derivedFacts)
            writer.println(s"\t\t*********************Forward chaining Query result: $result1")
            writer.println("-----------------------------------------------------------------")
        }

        {
            writer.println("---------------------mother--------------------------------------------")
            val query2 = Compound("mother", List(Atom("john"), Atom("susan")))
            val result3 = validateQuery(query2, derivedFacts)
            writer.println(s"\t\t*********************Forward chaining Query result2: $result3")
            writer.println("-----------------------------------------------------------------")
        }
    }

    def testAncestorQuery(rules: List[Rule])(using writer: PrinterWriter): Unit = {
        writer.println("-----------------------------------------------------------------")
        writer.println(s"testAncestorQuery")
        writer.println("-----------------------------------------------------------------")


        val query = Compound("ancestor", List(Atom("john"), Variable("Y")))
        val initialSubstitution: Substitution = Map()

        // Perform resolution
        val results = KnowledgeBase.solve(List(query), initialSubstitution)
        writer.println("----------------------ancestor-------------------------------------------")
        // Output the results
        results match {
            case Nil => writer.println("*********************No solutions found.")
            case solutions =>
            writer.println(s"*********************Solutions: ancestor")
            solutions.foreach(subst => writer.println(s"  $subst"))
        }
        writer.println("-----------------------------------------------------------------")
    }

    def testMotherQuery(rules: List[Rule])(using writer: PrinterWriter): Unit = {
        writer.println("-----------------------------------------------------------------")
        writer.println(s"testMotherQuery")
        writer.println("-----------------------------------------------------------------")

        val query = Compound("mother", List(Variable("X"), Atom("john")))
        val initialSubstitution: Substitution = Map()

        // Perform resolution
        val results = KnowledgeBase.solve(List(query), initialSubstitution)

        writer.println("-----------------------------------------------------------------")
        // Output the results
        results match {
            case Nil => writer.println("********************* No solutions found.")
            case solutions =>
            writer.println(s"********************* Solutions: mother")
            solutions.foreach(subst => writer.println(s"  $subst"))
        }
        writer.println("-----------------------------------------------------------------")
    }
    
    def testBachelorNotQuery(rules: List[Rule])(using writer: PrinterWriter): Unit = {
        // Knowledge Base
        writer.println("-----------------------------------------------------------------")
        writer.println(s"\ttestBachelorNotQuery")
        writer.println("-----------------------------------------------------------------")
        val facts: Set[Compound] = Set(
            Compound("male", List(Atom("john"))),
            Compound("male", List(Atom("peter"))),
            Compound("male", List(Atom("jim"))),
            Compound("female", List(Atom("lisa"))),
            Compound("female", List(Atom("susan"))),
            Compound("parent", List(Atom("john"), Atom("susan"))),
            Compound("parent", List(Atom("john"), Atom("mary"))),
            Compound("parent", List(Atom("lisa"), Atom("peter"))),
            Compound("married", List(Atom("jim")))
        )

        // Define the query function
        def query(facts: Set[Compound], goal: Compound): Boolean = {
            facts.exists(fact => unify(goal, fact, Map()).isDefined)
        }
        val query1 = Compound("bachelor", List(Atom("john")))
        val query2 = Compound("bachelor", List(Atom("peter")))
        val query3 = Compound("married", List(Atom("jim")))
        val result1 = backwardChaining(query1, rules, facts)
        writer.println("-----------------------------------------------------------------")
        writer.println(s"Query Result for backwardChaining $query1: $result1")
        writer.println("-----------------------------------------------------------------")
        val result2 = backwardChaining(query2, rules, facts)
        
        writer.println("-----------------------------------------------------------------")
        writer.println(s"Query Result for backwardChaining $query2: $result2")
        writer.println("-----------------------------------------------------------------")
        val result3 = backwardChaining(query3, rules, facts)
        writer.println("-----------------------------------------------------------------")
        writer.println(s"Query Result for backwardChaining $query3: $result3")
        writer.println("-----------------------------------------------------------------")

        // Test forward chaining
        val derivedFacts = forwardChaining(facts, rules)
        writer.println("-----------------------------------------------------------------")
        writer.println(s"Derived Facts bachelor forwardChaining: $derivedFacts")
        writer.println("-----------------------------------------------------------------")
        
        // Query for bachelor
        val bachelorQuery = Compound("bachelor", List(Atom("john")))
        val resultfc1 = query(derivedFacts, bachelorQuery)
        
        writer.println("-----------------------------------------------------------------")
        writer.println(s"bachelor john Result forwardChaining: $resultfc1")
        writer.println("-----------------------------------------------------------------")
        
        val bachelorQuery2 = Compound("married", List(Atom("jim")))
        val resultfc2 = query(derivedFacts, bachelorQuery2)
        writer.println("-----------------------------------------------------------------")
        writer.println(s"married jim Result forwardChaining: $resultfc2")
        writer.println("-----------------------------------------------------------------")
    }

    test("testMotherQuery") {
        testMotherQuery(KnowledgeBase.rules)
    }

    test("testAncestorQuery") {
        testAncestorQuery(KnowledgeBase.rules)
    }

    test("testForwardChaining") {
      testForwardChaining(KnowledgeBase.rules)
    }

    test("testBachelorNotQuery") {
      testBachelorNotQuery(KnowledgeBase.rules)
    }
}