import scala.annotation.tailrec
import java.io.PrintWriter

// Define the Term ADT
sealed trait Term
case class Atom(name: String) extends Term
case class Variable(name: String) extends Term
case class Compound(functor: String, args: List[Term]) extends Term
case class Not(term: Term) extends  Term
case class Cut(name: String) extends Term


// Define rules and knowledge base
sealed trait RuleBase
case class Rule(head: Compound, body: List[Term]) extends RuleBase
case class Fact(head: Term) extends RuleBase
//case class Rule(head: Term, body: List[Term]) extends Rule
// Substitution maps Variables to Terms
type Substitution = Map[Variable, Term]

var rules: List[Rule] = List(
    Rule(Compound("male", List(Atom("john"))), Nil),
    Rule(Compound("male", List(Atom("peter"))), Nil),
    Rule(Compound("male", List(Atom("paul"))), Nil),
    Rule(Compound("female", List(Atom("jenny"))), Nil),
    Rule(Compound("female", List(Atom("jen"))), Nil),
    Rule(Compound("female", List(Atom("granny"))), Nil),
    Rule(Compound("parent", List(Atom("jen"),Atom("jenny"))), Nil),
    Rule(Compound("parent", List(Atom("granny"),Atom("jen"))), Nil),
    Rule(Compound("married", List(Atom("paul"),Atom("paulina"))), Nil),

    Rule(Compound("grandmother", List(Variable("X"), Variable("Y"))), 
        List(Compound("female", List(Variable("X"))),
            Compound("parent", List(Variable("X"),Variable("Z"))),
            Compound("parent", List(Variable("Z"),Variable("Y"))))),

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
                Compound("not", List(Compound("married", List(Variable("X"), Variable("_")))))
            )),
    Rule(Compound("husband", List(Variable("X"),Variable("Y"))), List(
        Compound("male", List(Variable("X"))),
        Compound("married",List(Variable("X"),(Variable("Y"))))))
)

rules.toStream.flatMap {
  r => List(r.head)
}