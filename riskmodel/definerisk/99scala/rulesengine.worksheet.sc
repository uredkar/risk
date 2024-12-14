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
  )

class RuleContainer[F[_]](val data: F[Rule])(using functor: Functor[F]) {
  // Method to map over the inner Option
  def mapInner[B](f: Rule => B): F[B] = {
    functor.map(data) {
        case value => 
            val transformed : B = f(value)
            println(s"Mapping value: $value to \"$transformed\"") 
            transformed
        
    }
  }


}

def formatRule(r: Rule): String =
    s"rule ${r.head} ${r.body}"

def printRule[F[_]](container: F[Rule])(using functor: Functor[F]) = 
    functor.map(container)(r=> formatRule(r))

    

printRule(rules)

val listSafeContainer = new RuleContainer(rules)
listSafeContainer.mapInner(x => x.head)


trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

object Functor:
  def apply[F[_]](using functor: Functor[F]): Functor[F] = functor

  given Functor[List] with
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

  given Functor[Option] with
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

extension [F[_]](functor: Functor[F]) // Attach extensions to an explicit parameter
  def compose[G[_]](using Functor[G]): Functor[[A] =>> F[G[A]]] =
    new Functor[[A] =>> F[G[A]]]:
      def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
        functor.map(fa)(ga => summon[Functor[G]].map(ga)(f))


def transform[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[B] =
  summon[Functor[F]].map(fa)(f)


trait Monad[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // `map` can be derived from `flatMap` and `pure`
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

object Monad:
  given Monad[List] with
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

  given Monad[Option] with
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
