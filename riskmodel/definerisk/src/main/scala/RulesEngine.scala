package com.definerisk.core.models

import scala.annotation.tailrec

import io.circe.{Encoder, Decoder, Json}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.yaml.syntax._
import io.circe.yaml.parser
import io.circe.yaml.Printer
import io.circe._
import java.io.{File, FileWriter, PrintWriter}
import scala.io.Source


// Define the Term ADT
sealed trait Term
case class Atom(name: String) extends Term
case class Variable(name: String) extends Term
case class Compound(functor: String, args: List[Term]) extends Term
case class Not(innerQuery: Compound) extends  Term

// Define rules and knowledge base
case class Rule(head: Compound, body: List[Compound | Not])

// Substitution maps Variables to Terms
type Substitution = Map[Variable, Term]

// Encoders for Term

given Encoder[Term] with
  def apply(term: Term): Json = term match
    case Atom(name) => Json.obj("Atom" -> Json.fromString(name))
    case Variable(name) => Json.obj("Variable" -> Json.fromString(name))
    case Compound(functor, args) =>
      Json.obj(
        "Compound" -> Json.obj(
          "functor" -> Json.fromString(functor),
          "args" -> Json.arr(args.map(Encoder[Term].apply)*)
        )
      )
    case Not(innerQuery) =>
      Json.obj(
        "Not" -> Json.obj(
          "innerQuery" -> Encoder[Compound].apply(innerQuery)
        )
      )

  // Decoder for Term
given Decoder[Term] with
  def apply(c: HCursor): Decoder.Result[Term] =
    c.keys.flatMap(_.headOption) match
      case Some("Atom")     => c.downField("Atom").as[String].map(Atom.apply)
      case Some("Variable") => c.downField("Variable").as[String].map(Variable.apply)
      case Some("Compound") =>
        for
          functor <- c.downField("Compound").downField("functor").as[String]
          args <- c.downField("Compound").downField("args").as[List[Term]]
        yield Compound(functor, args)
      case Some("Not") =>
        c.downField("Not").downField("innerQuery").as[Compound].map(Not.apply)
      case _ => Left(DecodingFailure("Unknown Term type", c.history))

// Encoders and Decoders for other types
given Encoder[Compound] = deriveEncoder[Compound]
given Encoder[Not] = deriveEncoder[Not]
given Encoder[Compound | Not] with
  def apply(value: Compound | Not): Json = value match
    case c: Compound => Encoder[Compound].apply(c)
    case n: Not      => Encoder[Not].apply(n)

given Decoder[Compound] = deriveDecoder[Compound]
given Decoder[Not] = deriveDecoder[Not]
given Decoder[Compound | Not] with
  def apply(c: HCursor): Decoder.Result[Compound | Not] =
    if c.keys.exists(_.mkString.contains("functor")) then Decoder[Compound].apply(c)
    else Decoder[Not].apply(c)

given Encoder[Rule] = deriveEncoder[Rule]
given Decoder[Rule] = deriveDecoder[Rule]





def unify(term1: Term, term2: Term, subst: Substitution)(using writer: PrinterWriter): Option[Substitution] = {
  writer.println(s"with $term2 subst $subst")
  val subs = (term1, term2) match {
    case (Atom(name1), Atom(name2)) if name1 == name2 =>
      writer.println(s"** Unified atoms: $term1 = $term2")
      Some(subst)
    case (Variable(name), term) =>
      writer.println(s"*** Unifying variable $name with term $term")
      unifyVariable(Variable(name), term, subst)

    case (term, Variable(name)) =>
      writer.println(s"*** Unifying term $term with variable $name")
      unifyVariable(Variable(name), term, subst)

    case (Compound(f1, args1), Compound(f2, args2)) if f1 == f2 && args1.size == args2.size =>
       unifyArgs(args1, args2, subst).flatMap(newSubst =>
         if (newSubst == subst) Some(subst) else Some(newSubst)
      )
      //val args = unifyArgs(args1, args2, subst)
      //writer.println(s"*** Unifying compounds: term1 $term1 = term2 $term2 args = $args subst $subst")
      //args

    case other =>
      writer.println(s"\t\tFailed to unify $other with $term2")
      None
  }
  writer.println(s"\t\tunify returning subs $subs")
  subs
}



// Unify argument lists
def unifyVariable(v: Variable, t: Term, subst: Substitution)(using writer: PrinterWriter): Option[Substitution] = {
  writer.println(s"unifyVariable called: $v -> $t with $subst")
  if (subst.contains(v)) {
    writer.println(s"Variable $v is already substituted: ${subst(v)}")
    unify(subst(v), t, subst)
  } else if (occursCheck(v, t, subst)) {
    writer.println(s"Occurs-check failed for $v in $t")
    None
  }
  else if (t.isInstanceOf[Variable] && subst.contains(t.asInstanceOf[Variable])) {
    // If `t` is a variable and already substituted, unify with its substitution
    unify(v, subst(t.asInstanceOf[Variable]), subst)
  } else if (t == v) {
    // Variable equals the term
    Some(subst)
  }  else {
    // Add the new substitution
    val newSubst = subst + (v -> t)
    writer.println(s"Adding substitution: $v -> $t. New substitution: $newSubst")
    Some(newSubst)
  }
}


def unifyArgs(args1: List[Term], args2: List[Term], subst: Substitution)(using writer: PrinterWriter): Option[Substitution] = {
  writer.println(s"Attempting to unify argument lists: $args1 with $args2 using substitution $subst")
  (args1, args2) match {
    case (Nil, Nil) =>
      writer.println(s"UnifyArgs: Both argument lists are empty. Returning $subst")
      Some(subst)

    case (arg1 :: rest1, arg2 :: rest2) =>
      writer.println(s"UnifyArgs: Unifying $arg1 with $arg2")
      unify(arg1, arg2, subst) match {
        case Some(newSubst) =>
          writer.println(s"UnifyArgs: Successfully unified $arg1 with $arg2. New substitution: $newSubst")
          unifyArgs(rest1, rest2, newSubst)
        case None =>
          writer.println(s"UnifyArgs: Failed to unify $arg1 with $arg2. Aborting.")
          None
      }

    case _ =>
      writer.println(s"UnifyArgs: Argument lists are of different lengths or mismatched. Aborting.")
      None
  }
}



def occursCheck(variable: Variable, term: Term, subst: Substitution)(using writer: PrinterWriter): Boolean = {
  def occurs(variable: Variable, term: Term): Boolean = {
    term match {
      case `variable` => true
      case Compound(_, args) => args.exists(arg => occurs(variable, applySubstitution(arg, subst)))
      case v: Variable => subst.get(v).exists(subTerm => occurs(variable, subTerm))
    
      case _ => false
    }
  }
  occurs(variable, term)
}
private val substitutionCache = scala.collection.mutable.Map[Term, Term]()
def applySubstitution_old(term: Term, subst: Substitution)(using writer: PrinterWriter): Term = {
  writer.println(s"\tapplySubstitution term $term subst $subst")
  //if (depth > 100) throw new StackOverflowError("Recursion depth exceeded in applySubstitution")
  if (substitutionCache.contains(term)) {
    substitutionCache(term)
  } 
  else {
    val substituted = applySubstitution_old(term, subst)
    substitutionCache(term) = substituted
    substituted
  }
  
}

def applySubstitution_old2(term: Term, subst: Substitution): Term = {
  import scala.collection.mutable

  // Stack to keep track of terms to process
  val stack = mutable.Stack[(Term, List[Term])]()

  // Map to store substitution results for compound terms
  val cache = mutable.Map[Term, Term]()

  // Push the initial term to the stack
  stack.push((term, Nil))

  while (stack.nonEmpty) {
    val (currentTerm, processedArgs) = stack.pop()

    currentTerm match {
      case v: Variable =>
        // Substitute the variable if it exists in the substitution map
        val substituted = subst.getOrElse(v, v)
        if (substituted == v) {
          // No substitution needed
          cache(currentTerm) = v
        } else {
          // Push the substituted term back to process
          stack.push((substituted, Nil))
        }

      case Compound(f, args) =>
        if (args.isEmpty) {
          // All arguments processed: reconstruct the term
          cache(currentTerm) = Compound(f, processedArgs.reverse)
        } else {
          // Push the remaining arguments to the stack
          val headArg = args.head
          val tailArgs = args.tail
          stack.push((Compound(f, tailArgs), processedArgs)) // Remaining args
          stack.push((headArg, Nil))                         // Current arg
        }

      case atom =>
        // Atoms remain unchanged
        cache(currentTerm) = atom
    }
  }

  // Return the substituted term
  cache(term)
}





def applySubstitution(term: Term, subst: Substitution): Term = {
  def substituteCompound(args: List[Term], acc: List[Term]): List[Term] = {
    args match {
      case Nil => acc.reverse
      case head :: tail =>
        val substitutedHead = applySubstitution(head, subst)
        substituteCompound(tail, substitutedHead :: acc)
    }
  }

  term match {
    case v: Variable =>
      subst.getOrElse(v, v)

    case Compound(f, args) =>
      val substitutedArgs = substituteCompound(args, Nil)
      Compound(f, substitutedArgs)

    case atom => atom
  }
}


def backwardChaining(
    query: Term,
    rules: List[Rule],
    facts: Set[Compound],
    visited: Set[Term] = Set()
)(using writer: PrinterWriter): Boolean = {
  writer.println("-----------------------------------------------------------------")
  writer.println(s"backwardChaining: $facts\n\t $visited")
  writer.println("-----------------------------------------------------------------")
  // Prevent infinite loops
  if (visited.contains(query)) return false

  query match {
    case compound: Compound =>
      // Check if the query can be unified with any fact
      if (facts.exists(f => unify(compound, f, Map()).isDefined)) true
      else {
        // Check if the query can be derived using rules
        rules.exists { rule =>
          unify(rule.head, compound, Map()).exists { subst =>
            rule.body.forall {
              case Not(innerQuery) =>
                // For `Not`, check that `innerQuery` cannot be proven
                !backwardChaining(innerQuery, rules, facts, visited + query)
              case subQuery =>
                // Standard query resolution
                backwardChaining(applySubstitution(subQuery, subst), rules, facts, visited + query)
            }
          }
        }
      }
    case _ => false // Unsupported query type
  }
}


def forwardChaining(facts: Set[Compound], rules: List[Rule])(using writer: PrinterWriter): Set[Compound] = {
  writer.println("-----------------------------------------------------------------")
  writer.println(s"forwardChaining: $facts")
  writer.println("-----------------------------------------------------------------")
  val newFacts: Set[Compound] = rules.flatMap { rule =>
    // Check if the rule body is satisfied
    if rule.body.forall {
        case Not(innerQuery) =>
          // Negation: Succeeds if inner query fails
          val negationResult = !facts.exists(f => unify(innerQuery, f, Map()).isDefined)
          writer.println(s"Negation for $innerQuery: $negationResult")
          negationResult
        case subQuery =>
          // Normal query: Succeeds if it unifies with existing facts
          val positiveResult = facts.exists(f => unify(subQuery, f, Map()).isDefined)
          writer.println(s"SubQuery $subQuery in facts: $positiveResult")
          positiveResult
      } then
          // Add the head of the rule if the body is satisfied
          val unifiedFact = unify(rule.head, rule.head, Map()).map(_ => rule.head)
          writer.println(s"Applying rule: $rule, Unified fact: $unifiedFact")
          unifiedFact.toList
    else 
      Nil
  }.toSet

  // If no new facts are derived, return the current set of facts
  if (newFacts.subsetOf(facts)) 
    writer.println("---------> No new facts end forward chaining <------------------")
    facts
  else 
    forwardChaining(facts ++ newFacts, rules)
}


def validateQuery(query: Compound, facts: Set[Compound])(using writer: PrinterWriter): Boolean = {
  facts.exists(fact => unify(query, fact, Map()).isDefined)
}

// Knowledge base to store rules
object KnowledgeBase {
 val rules: List[Rule] = List(
    Rule(Compound("male", List(Atom("john"))), Nil),
    Rule(Compound("male", List(Atom("peter"))), Nil),
    Rule(Compound("female", List(Atom("lisa"))), Nil),
    Rule(Compound("female", List(Atom("susan"))), Nil),
    Rule(Compound("female", List(Atom("monia"))), Nil),
    
    Rule(Compound("parent", List(Atom("john"), Atom("susan"))), Nil),
    Rule(Compound("parent", List(Atom("monia"), Atom("john"))), Nil),
    Rule(Compound("parent", List(Atom("susan"), Atom("lisa"))), Nil),
    Rule(Compound("parent", List(Atom("john"), Atom("mary"))), Nil),
    Rule(Compound("parent", List(Atom("mary"), Atom("peter"))), Nil),
    Rule(Compound("mother", List(Variable("X"), Variable("Y"))), 
      List(Compound("female", List(Variable("X"))),
           Compound("parent", List(Variable("X"),Variable("Y"))))),
    Rule(Compound("ancestor", List(Variable("X"), Variable("Y"))), List(Compound("parent", List(Variable("X"), Variable("Y"))))),

    Rule(Compound("ancestor", List(Variable("X"), Variable("Y"))), 
        List(Compound("parent", List(Variable("X"), Variable("Z"))), 
             Compound("cut", Nil), 
             Compound("ancestor", List(Variable("Z"), Variable("Y"))))),
    Rule(Compound("bachelor", List(Variable("X"))), List(
      Compound("male", List(Variable("X"))),
      Not(Compound("female", List(Variable("X"))))
    )),
    Rule(Compound("bachelor", List(Variable("X"))), List(
      Compound("male", List(Variable("X"))),
      Not(Compound("married", List(Variable("X"))))
    ))
  )

  def query(goal: Compound)(using writer: PrinterWriter): List[Substitution] = 
    val initialSubstitution: Substitution = Map()
    solve(List(goal), initialSubstitution)

  def solve(goals: List[Term], subst: Substitution)(using writer: PrinterWriter): List[Substitution] = {
    writer.println("-----------------------------------------------------------------")
    writer.println(s"Solving goals: $goals with subst: $subst")
    writer.println("-----------------------------------------------------------------")
    goals match {
      case Nil =>
        writer.println(s"All goals satisfied with subst: $subst")
        List(subst) // No more goals to solve
      case Compound("cut",Nil) :: rest => 
        writer.println("\nCutting...")
        solve(rest,subst)
      case goal :: rest =>
        rules.flatMap { rule =>
            writer.println(s"Trying to unify goal: \n\t$goal with \n\trule: $rule")
            unify(goal, rule.head, subst).toList.flatMap { unifiedSubst  =>
              writer.println(s"\n\n!!!!! Unified!!!!\n\n New subst: $unifiedSubst . Solving rule body: ${rule.body} and rest: $rest")
              val newGoals = rule.body.map(applySubstitution(_, unifiedSubst)) ++ rest
              solve(newGoals, unifiedSubst)
          }
           
        }
    }
  }
}

def saveRules(rules: List[Rule], file: File): Unit =
  import io.circe.syntax._
  import io.circe.yaml.syntax._
  
  val yamlPrinter = Printer.spaces2
  val yamlString = yamlPrinter.pretty(rules.asJson)
  val writer = new PrintWriter(file)
  try writer.write(yamlString)
  finally writer.close()

def loadRules(file: File): Either[io.circe.Error, List[Rule]] =
    val yamlContent = Source.fromFile(file).mkString
    parser.parse(yamlContent).flatMap(_.as[List[Rule]])






@main def testRules() = 
  val writer = new PrintWriter(ConfigReader.rulesEngineLogDirectory)
  println(s"output is in this file ${ConfigReader.rulesEngineLogDirectory}")
  val file = new File("rules.yaml")
  saveRules(KnowledgeBase.rules,file)
  val rules = loadRules(file) match {
    case Right(loadedRules) => println(s"loadedRules loaded: $loadedRules")
      //testMotherQuery(loadedRules)
      //testAncestorQuery(loadedRules)
      //testForwardChaining(loadedRules)
      //testBachelorNotQuery(loadedRules)
    case Left(error)            => writer.println(s"Failed to load loadedRules: $error")
      error
  }
  writer.close()
  
