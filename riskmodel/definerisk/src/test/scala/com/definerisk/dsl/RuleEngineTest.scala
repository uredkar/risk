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
    given PrinterWriter with
        def println(s: String): Unit = writer.println(s)
    
    override def afterAll(): Unit = {
        writer.close()
        super.afterAll()
    }        
    
    override def beforeEach(): Unit = {
        testPrinterWriter = new TestPrinterWriter {}
    }

    override def afterEach(): Unit = {
        testPrinterWriter.writer.close()
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
            val query2 = Compound("mother", List(Atom("monia"), Atom("john")))
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
        val facts: Set[Compound] = Set(
        )
        val query = Compound("mother", List(Variable("X"), Atom("john")))
        val initialSubstitution: Substitution = Map()
        val results1 = backwardChaining(query, rules)
        // Perform resolution
        val results2 = KnowledgeBase.solve(List(query), initialSubstitution)

        writer.println("-----------------------------------------------------------------")
        // Output the results
        writer.println(s"testMotherQuery result backward chaining $results1 solve $results1")
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
        val result1 = backwardChaining(query1, rules)
        writer.println("-----------------------------------------------------------------")
        writer.println(s"Query Result for backwardChaining $query1: $result1")
        writer.println("-----------------------------------------------------------------")
        val result2 = backwardChaining(query2, rules)
        
        writer.println("-----------------------------------------------------------------")
        writer.println(s"Query Result for backwardChaining $query2: $result2")
        writer.println("-----------------------------------------------------------------")
        val result3 = backwardChaining(query3, rules)
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
    
    test("occurs working") {
        //val writer = new StringWriter()
        //given PrinterWriter = new PrintWriter(writer)

        val x = Variable("x")
        val y = Variable("y")
        val z = Variable("z")
        val f = Compound("f", List(x))

        // Basic Cases
        occursCheck(x, x, Map.empty) shouldBe false
        occursCheck(x, y, Map.empty) shouldBe false

        // Compound Terms
        occursCheck(x, f, Map.empty) shouldBe false
        occursCheck(x, Compound("f", List(y)), Map.empty) shouldBe false

        // Substituted Terms
        occursCheck(x, Compound("f", List(y)), Map(y -> x)) shouldBe false
        occursCheck(x, Compound("f", List(y)), Map(y -> z)) shouldBe false
    }

    test("testMotherQuery") {
        testMotherQuery(KnowledgeBase.rules)
    }
/*
    test("testAncestorQuery") {
        testAncestorQuery(KnowledgeBase.rules)
    }

    test("testForwardChaining") {
      testForwardChaining(KnowledgeBase.rules)
    }

    test("testBachelorNotQuery") {
      testBachelorNotQuery(KnowledgeBase.rules)
    }
  
    test("unify should successfully unify two atoms with the same name") {
        val atom1 = Atom("A")
        val atom2 = Atom("A")
        val subst = Map.empty[Variable, Term]

        given PrinterWriter = testPrinterWriter

        val result = unify(atom1, atom2, subst)
        result shouldBe Some(subst)
        testPrinterWriter.getOutput should include("Unified atoms")
        writer.println(testPrinterWriter.getOutput)
  }

    test("unify should fail to unify two atoms with different names") {
        val atom1 = Atom("A")
        val atom2 = Atom("B")
        val subst = Map.empty[Variable, Term]

        given PrinterWriter = testPrinterWriter

        val result = unify(atom1, atom2, subst)
        result shouldBe None
        testPrinterWriter.getOutput should include("Failed to unify")
        writer.println(testPrinterWriter.getOutput)
    }

    test("unifyVariable should add a new substitution") {
        //val writer = new StringWriter()
        given PrinterWriter = testPrinterWriter

        val variable = Variable("X")
        val term = Atom("A")
        val subst = Map.empty[Variable, Term]

        val result = unifyVariable(variable, term, subst)
        result shouldBe Some(subst + (variable -> term))
        //println(s"------------> ${testPrinterWriter.getOutput}")
        testPrinterWriter.getOutput should include("unifyVariable")
        writer.println(testPrinterWriter.getOutput)
    }

    test("unifyArgs should unify matching argument lists") {
        val args1 = List(Atom("A"), Atom("B"))
        val args2 = List(Atom("A"), Atom("B"))
        val subst = Map.empty[Variable, Term]

        given PrinterWriter = testPrinterWriter

        val result = unifyArgs(args1, args2, subst)
        result shouldBe Some(subst)
        testPrinterWriter.getOutput should include("UnifyArgs: Successfully unified")
        writer.println(testPrinterWriter.getOutput)
    }

   

    
    test("handle compound terms") {
        val subst = Map(Variable("x") -> Atom("a"))
        applySubstitution(Compound("f", List(Variable("x"), Atom("b"))), subst) shouldBe Compound("f", List(Atom("a"), Atom("b")))
    }
    
    
    test("prevent infinite recursion in cyclic substitutions") {
        val subst = Map(Variable("x") -> Compound("f", List(Variable("x"))))
        applySubstitution(Variable("x"), subst)
        
    }
    
    test("substitute variables correctly") {
        val subst = Map(Variable("x") -> Atom("a"))
        applySubstitution(Variable("x"), subst) shouldBe Atom("a")
    }
    
    test("backwardChaining should validate a query using facts and rules") {
        val facts = Set(Compound("parent", List(Atom("John"), Atom("Mary"))))
        val query = Compound("parent", List(Atom("John"), Atom("Mary")))
        val rules = List.empty[Rule]

        given PrinterWriter = testPrinterWriter

        val result = backwardChaining(query, rules, facts)
        result shouldBe true
        testPrinterWriter.getOutput should include("backwardChaining")
    }

    test("forwardChaining should derive new facts using rules") {
        val facts = Set(Compound("human", List(Atom("John"))))
        val rules = List(
        Rule(Compound("mortal", List(Atom("John"))), List(Compound("human", List(Atom("John")))))
        )

        given PrinterWriter = testPrinterWriter

        val result = forwardChaining(facts, rules)
        result should contain(Compound("mortal", List(Atom("John"))))
        testPrinterWriter.getOutput should include("forwardChaining")
    }
    */  
}