import scala.annotation.tailrec

// Define the Term ADT
sealed trait Term
case class Atom(name: String) extends Term
case class Variable(name: String) extends Term
case class Compound(functor: String, args: List[Term]) extends Term

// Substitution maps Variables to Terms
type Substitution = Map[Variable, Term]


def unify(term1: Term, term2: Term, subst: Substitution): Option[Substitution] = {
  println(s"Unifying $term1 with $term2 \n\t\t\tsubst $subst")
  val subs = (term1, term2) match {
    case (Atom(name1), Atom(name2)) if name1 == name2 =>
      println(s"** Unified atoms: $term1 = $term2")
      Some(subst)
    case (Variable(name), term) =>
      println(s"*** Unifying variable $name with term $term")
      unifyVariable(Variable(name), term, subst)

    case (term, Variable(name)) =>
      println(s"*** Unifying term $term with variable $name")
      unifyVariable(Variable(name), term, subst)

    case (Compound(f1, args1), Compound(f2, args2)) if f1 == f2 && args1.size == args2.size =>
      val args = unifyArgs(args1, args2, subst)
      println(s"*** Unifying compounds: term1 $term1 = term2 $term2 args = $args subst $subst")
      args

    case other =>
      println(s"Failed to unify $other $term1 with $term2")
      None
  }
  println(s"unify returning subs $subs")
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
  (args1, args2) match {
    case (Nil, Nil) => Some(subst)
    case (arg1 :: rest1, arg2 :: rest2) =>
      unify(arg1, arg2, subst).flatMap(newSubst => unifyArgs(rest1, rest2, newSubst))
    case _ => None
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
  println(s"\n\n<>>>>>>>>>>>>>>>> applySubstitution term $term subst $subst \n\n")
  term match {
    case v: Variable => subst.getOrElse(v, v)
    case Compound(f, args) => Compound(f, args.map(arg => applySubstitution(arg, subst)))
    case atom => atom
  }
}
// Define rules and knowledge base
case class Rule(head: Compound, body: List[Compound])


// Knowledge base to store rules
object KnowledgeBase {
 val rules: List[Rule] = List(
    Rule(Compound("parent", List(Atom("john"), Atom("susan"))), Nil),
    Rule(Compound("parent", List(Atom("susan"), Atom("lisa"))), Nil),
    Rule(Compound("parent", List(Atom("john"), Atom("mary"))), Nil),
    Rule(Compound("parent", List(Atom("mary"), Atom("peter"))), Nil),
    Rule(Compound("ancestor", List(Variable("X"), Variable("Y"))), List(Compound("parent", List(Variable("X"), Variable("Y"))))),
    Rule(
      Compound("ancestor", List(Variable("X"), Variable("Y"))),
      List(
        Compound("parent", List(Variable("X"), Variable("Z"))),
        Compound("ancestor", List(Variable("Z"), Variable("Y")))
      )
    )
  )

  def query(goal: Compound): List[Substitution] = solve(List(goal), Map.empty)

  def solve(goals: List[Term], subst: Substitution): List[Substitution] = {
    println(s"Solving goals: $goals with subst: $subst")
    goals match {
      case Nil =>
        println(s"All goals satisfied with subst: $subst")
        List(subst) // No more goals to solve
      case goal :: rest =>
        rules.flatMap { rule =>
            println(s"Trying to unify goal: $goal with rule: $rule")
            unify(goal, rule.head, subst).toList.flatMap { unifiedSubst  =>
              println(s"\n\n!!!!! Unified!!!!\n\n New subst: $unifiedSubst . Solving rule body: ${rule.body} and rest: $rest")
              val newGoals = rule.body.map(applySubstitution(_, unifiedSubst)) ++ rest
              solve(newGoals, unifiedSubst)
          }
           
        }
    }
  }
}

@main def runQuery(): Unit = {
  val query = Compound("ancestor", List(Atom("john"), Variable("Y")))
  val results = KnowledgeBase.query(query)
  results.foreach(println)
  //println(s"Results: $results")
}