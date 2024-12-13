import scala.annotation.tailrec
import java.io.PrintWriter
// Define the Term ADT
sealed trait Term
case class Atom(name: String) extends Term
case class Variable(name: String) extends Term
case class Compound(functor: String, args: List[Term]) extends Term

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
// Define rules and knowledge base
case class Rule(head: Compound, body: List[Compound])

def testMotherQuery(rules: List[Rule]): Unit = {
  val query = Compound("mother", List(Variable("X"), Atom("john")))
  val initialSubstitution: Substitution = Map()

  // Perform resolution
  val results = KnowledgeBase.solve(List(query), initialSubstitution)

  // Output the results
  results match {
    case Nil => println("No solutions found.")
    case solutions =>
      println(s"Solutions:")
      solutions.foreach(subst => println(s"  $subst"))
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
    Rule(
      Compound("ancestor", List(Variable("X"), Variable("Y"))),
      List(
        Compound("parent", List(Variable("X"), Variable("Z"))),
        Compound("ancestor", List(Variable("Z"), Variable("Y")))
      )
    )
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



  //val query = Compound("ancestor", List(Atom("john"), Variable("Y")))
  //val results = KnowledgeBase.query(query)
  //results.foreach(writer.println)
  writer.println("-----------------------------------------------------------------")
  //val query2 = Compound("mother", List(Variable("X"),Atom("John")))
  val query2 = Compound("mother", List(Variable("X"), Atom("john")))
  val results2 = KnowledgeBase.query(query2)
  writer.println("Solution:")
  results2.foreach(writer.println)
  writer.close()
  //writer.println(s"Results: $results")
}