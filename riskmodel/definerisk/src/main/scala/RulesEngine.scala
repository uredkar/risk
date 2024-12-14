import scala.annotation.tailrec
import java.io.PrintWriter
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

val writer = new PrintWriter(ConfigReader.rulesEngineLogDirectory)

def unify(term1: Term, term2: Term, subst: Substitution): Option[Substitution] = {
  writer.println(s"with $term2 subst $subst")
  val subs = (term1, term2) match {
    case (Atom(name1), Atom(name2)) if name1 == name2 =>
      //writer.println(s"** Unified atoms: $term1 = $term2")
      Some(subst)
    case (Variable(name), term) =>
      //writer.println(s"*** Unifying variable $name with term $term")
      unifyVariable(Variable(name), term, subst)

    case (term, Variable(name)) =>
      //writer.println(s"*** Unifying term $term with variable $name")
      unifyVariable(Variable(name), term, subst)

    case (Compound(f1, args1), Compound(f2, args2)) if f1 == f2 && args1.size == args2.size =>
      val args = unifyArgs(args1, args2, subst)
      writer.println(s"*** Unifying compounds: term1 $term1 = term2 $term2 args = $args subst $subst")
      args

    case other =>
      writer.println(s"\t\tFailed to unify $other with $term2")
      None
  }
  writer.println(s"\t\tunify returning subs $subs")
  subs
}



// Unify argument lists
def unifyVariable(v: Variable, t: Term, subst: Substitution): Option[Substitution] = {
  if (subst.contains(v)) {
    // If the variable is already in the substitution, unify with its value
    unify(subst(v), t, subst)
  } else if (t.isInstanceOf[Variable] && subst.contains(t.asInstanceOf[Variable])) {
    // If `t` is a variable and already substituted, unify with its substitution
    unify(v, subst(t.asInstanceOf[Variable]), subst)
  } else if (t == v) {
    // Variable equals the term
    Some(subst)
  } else if (occursCheck(v, t, subst)) {
    // Occurs check prevents infinite recursion
    None
  } else {
    // Add the new substitution
    Some(subst + (v -> t))
  }
}


def unifyArgs(args1: List[Term], args2: List[Term], subst: Substitution): Option[Substitution] = {
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



def occursCheck(variable: Variable, term: Term, subst: Substitution): Boolean = {
  def occurs(variable: Variable, term: Term): Boolean = {
    term match {
      case `variable` => true
      case Compound(_, args) => args.exists(arg => occurs(variable, applySubstitution(arg, subst)))
      case _ => false
    }
  }
  occurs(variable, term)
}

def applySubstitution(term: Term, subst: Substitution): Term = {
  writer.println(s"\tapplySubstitution term $term subst $subst")
  term match {
    case v: Variable => subst.getOrElse(v, v)
    case Compound(f, args) => Compound(f, args.map(arg => applySubstitution(arg, subst)))
    case atom => atom
  }
}

def testBachelorNotQuery(): Unit = {
  // Knowledge Base
  val rules: List[Rule] = List(
    // Bachelor rule with negation
    Rule(Compound("bachelor", List(Variable("X"))), List(
      Compound("male", List(Variable("X"))),
      Not(Compound("married", List(Variable("X"))))
    ))
  )

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
  println(s"Query Result for backwardChaining $query1: $result1")

  val result2 = backwardChaining(query2, rules, facts)
  println(s"Query Result for backwardChaining $query2: $result2")

  val result3 = backwardChaining(query3, rules, facts)
  println(s"Query Result for backwardChaining $query3: $result3")


  // Test forward chaining
  val derivedFacts = forwardChaining(facts, rules)
  println(s"Derived Facts bachelor forwardChaining: $derivedFacts")

  // Query for bachelor
  val bachelorQuery = Compound("bachelor", List(Atom("john")))
  val resultfc1 = query(derivedFacts, bachelorQuery)
  println(s"bachelorQuery Result forwardChaining: $resultfc1")

  val bachelorQuery2 = Compound("married", List(Atom("jim")))
  val resultfc2 = query(derivedFacts, bachelorQuery2)
  println(s"bachelorQuery Result forwardChaining: $resultfc2")
  
}

def testAncestorQuery(rules: List[Rule]): Unit = {
  val query = Compound("ancestor", List(Atom("john"), Variable("Y")))
  val initialSubstitution: Substitution = Map()

  // Perform resolution
  val results = KnowledgeBase.solve(List(query), initialSubstitution)

  // Output the results
  results match {
    case Nil => println("No solutions found.")
    case solutions =>
      println(s"Solutions: ancestor")
      solutions.foreach(subst => println(s"  $subst"))
  }
}

def testMotherQuery(rules: List[Rule]): Unit = {
  val query = Compound("mother", List(Variable("X"), Atom("john")))
  val initialSubstitution: Substitution = Map()

  // Perform resolution
  val results = KnowledgeBase.solve(List(query), initialSubstitution)

  // Output the results
  results match {
    case Nil => println("No solutions found.")
    case solutions =>
      println(s"Solutions: mother")
      solutions.foreach(subst => println(s"  $subst"))
  }
}

def backwardChaining(
    query: Term,
    rules: List[Rule],
    facts: Set[Compound],
    visited: Set[Term] = Set()
): Boolean = {
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


def forwardChaining(facts: Set[Compound], rules: List[Rule]): Set[Compound] = {
  val newFacts: Set[Compound] = rules.flatMap { rule =>
    // Check if the rule body is satisfied
    if rule.body.forall {
        case Not(innerQuery) =>
          // Negation: Succeeds if inner query fails
          val negationResult = !facts.exists(f => unify(innerQuery, f, Map()).isDefined)
          //println(s"Negation for $innerQuery: $negationResult")
          negationResult
        case subQuery =>
          // Normal query: Succeeds if it unifies with existing facts
          val positiveResult = facts.exists(f => unify(subQuery, f, Map()).isDefined)
          //println(s"SubQuery $subQuery in facts: $positiveResult")
          positiveResult
      } then
      // Add the head of the rule if the body is satisfied
      val unifiedFact = unify(rule.head, rule.head, Map()).map(_ => rule.head)
      //println(s"Applying rule: $rule, Unified fact: $unifiedFact")
      unifiedFact.toList
    else Nil
  }.toSet

  // If no new facts are derived, return the current set of facts
  if (newFacts.subsetOf(facts)) facts
  else forwardChaining(facts ++ newFacts, rules)
}


def validateQuery(query: Compound, facts: Set[Compound]): Boolean = {
  facts.exists(fact => unify(query, fact, Map()).isDefined)
}

def testForwardChaining(rules: List[Rule]) = {

  val facts = Set(
    Compound("male", List(Atom("john"))),
    Compound("female", List(Atom("susan"))),
    Compound("parent", List(Atom("john"), Atom("susan")))
  )

  val rules = List(
    Rule(
      Compound("mother", List(Variable("X"), Variable("Y"))),
      List(
        Compound("female", List(Variable("X"))),
        Compound("parent", List(Variable("X"), Variable("Y")))
      )
    ),
    Rule(
      Compound("ancestor", List(Variable("X"), Variable("Y"))),
      List(Compound("parent", List(Variable("X"), Variable("Y"))))
    ),
    Rule(
      Compound("ancestor", List(Variable("X"), Variable("Y"))),
      List(
        Compound("parent", List(Variable("X"), Variable("Z"))),
        Compound("ancestor", List(Variable("Z"), Variable("Y")))
      )
    ),
     // "Not" Example: If X is not female, X is a bachelor
    Rule(Compound("bachelor", List(Variable("X"))), List(
      Compound("male", List(Variable("X"))),
      Not(Compound("female", List(Variable("X"))))
    ))
  )

  //val derivedFactsNot = forwardChainingWithNot(facts, rules)
  val derivedFacts = forwardChaining(facts, rules)
  println(s"derivedFacts $derivedFacts")
  {
    val query = Compound("ancestor", List(Atom("john"), Atom("susan")))
    val result1 = validateQuery(query, derivedFacts)
    println(s"Forward chaining Query result: $result1")
  }

  {
    val query2 = Compound("mother", List(Atom("john"), Atom("susan")))
    val result3 = validateQuery(query2, derivedFacts)
    println(s"Forward chaining Query result2: $result3")
  }
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
             Compound("ancestor", List(Variable("Z"), Variable("Y")))))
    /*
    Rule(
      Compound("ancestor", List(Variable("X"), Variable("Y"))),
      List(
        Compound("parent", List(Variable("X"), Variable("Z"))),
        Compound("ancestor", List(Variable("Z"), Variable("Y")))
      )
    )*/
  )

  def query(goal: Compound): List[Substitution] = 
    val initialSubstitution: Substitution = Map()
    solve(List(goal), initialSubstitution)

  def solve(goals: List[Term], subst: Substitution): List[Substitution] = {
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
            //writer.println(s"Trying to unify goal: \n\t$goal with \n\trule: $rule")
            unify(goal, rule.head, subst).toList.flatMap { unifiedSubst  =>
              writer.println(s"\n\n!!!!! Unified!!!!\n\n New subst: $unifiedSubst . Solving rule body: ${rule.body} and rest: $rest")
              val newGoals = rule.body.map(applySubstitution(_, unifiedSubst)) ++ rest
              solve(newGoals, unifiedSubst)
          }
           
        }
    }
  }
}

@main def runQuery(): Unit = {

  testMotherQuery(KnowledgeBase.rules)
  testAncestorQuery(KnowledgeBase.rules)
  testForwardChaining(KnowledgeBase.rules)
  testBachelorNotQuery()

  
  writer.println("-----------------------------------------------------------------")
  //val query2 = Compound("mother", List(Variable("X"),Atom("John")))
  val query2 = Compound("mother", List(Variable("X"), Atom("john")))
  val results2 = KnowledgeBase.query(query2)
  writer.println("Solution:")
  results2.foreach(writer.println)
  writer.close()
  //writer.println(s"Results: $results")
}
