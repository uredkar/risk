import com.definerisk.core.models.{*,given }
type Substitution = Map[Variable, Term]

var rules: List[Rule] = List(
    Rule(Compound("male", List(Atom("john"))), Nil),
    Rule(Compound("male", List(Atom("peter"))), Nil),
    Rule(Compound("male", List(Atom("paul"))), Nil),
    Rule(Compound("male", List(Atom("vick"))), Nil),
    Rule(Compound("female", List(Atom("jenny"))), Nil),
    Rule(Compound("female", List(Atom("jen"))), Nil),
    Rule(Compound("female", List(Atom("granny"))), Nil),
    Rule(Compound("parent", List(Atom("jen"),Atom("jenny"))), Nil),
    Rule(Compound("parent", List(Atom("granny"),Atom("jen"))), Nil),
    Rule(Compound("married", List(Atom("paul"),Atom("paulina"))), Nil),
    Rule(Compound("married", List(Atom("vick"),Atom("victoria"))), Nil),
    
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
    
    
object Unification {
    def unify(x: Term, y: Term, subst: Substitution): Option[Substitution] = 
        //println(s"unify $x $y $subst")
        (x, y) match {
            case (a1: Atom, a2: Atom) if a1 == a2 =>
                Some(subst) // Atoms match
            case (v: Variable, t) =>
                unifyVariable(v, t, subst)
            case (t, v: Variable) =>
                unifyVariable(v, t, subst)
            case (Compound(f1, args1), Compound(f2, args2)) if f1 == f2 && args1.size == args2.size =>
                unifyArgs(args1, args2, subst)
            case _ =>
                None // No match
    }

    def unifyVariable(v: Variable, t: Term, subst: Substitution): Option[Substitution] = {
        subst.get(v) match {
            case Some(value) =>
                // `v` already has a substitution, unify the substitution value with `t`
                println(s"Variable $v already has substitution: $value. Attempting to unify with $t.")
                unify(value, t, subst)

            case None =>
                t match {
                    case Variable(_) if subst.contains(t.asInstanceOf[Variable]) =>
                        // `t` is a variable that already has a substitution; resolve it recursively
                        val resolved = subst(t.asInstanceOf[Variable])
                        println(s"Resolving other variable $t (substituted as $resolved) to prevent circular substitution.")
                        unify(v, resolved, subst)

                    case _ if occursCheck(v, t, subst) =>
                        // Occurs check: prevent circular substitutions
                        println(s"Occurs check failed for variable $v in term $t. Unification aborted.")
                        None

                    case _ =>
                        // Add a new substitution
                        println(s"Adding new substitution: $v -> $t.")
                        Some(subst + (v -> t))
                }
        }
    }

    
    def occursCheck(v: Variable, t: Term, subst: Substitution): Boolean = t match {
        case `v` =>
            true // Variable can unify with itself (no circular reference)
        case Compound(_, args) =>
            args.exists(arg => occursCheck(v, arg, subst)) // Check recursively for cycles in compound terms
        case other: Variable =>
            // Check the substitution of `other` variable (if it exists) recursively
            subst.get(other).exists(resolved => occursCheck(v, resolved, subst))            
        //case Variable(_) => subst.get(t.asInstanceOf[Variable]).exists(occursCheck(v, _, subst)) // Check substitutions
        case _ => false
    }

    def unifyArgs(args1: List[Term], args2: List[Term], subst: Substitution): Option[Substitution] =
        //println(s"unifyArgs $args1 $args2 $subst")
        (args1, args2) match {
            case (Nil, Nil) => Some(subst)
            case (head1 :: tail1, head2 :: tail2) =>
                unify(head1, head2, subst).flatMap(newSubst => unifyArgs(tail1, tail2, newSubst))
            case _ => None
        }
}

object RulesEngine {
    var facts: Set[Term] = Set.empty // Stores known facts
    
    private var variableCounter = 0
    def resolve(goal: Term, rules: List[Rule], visited: Set[(Term, Substitution)], subst: Substitution): List[Substitution] = {
        if (visited.contains((goal, subst))) {
            Nil // Avoid revisiting the same goal with the same substitution
        } else {
            val newVisited = visited + ((goal, subst)) // Track current goal and substitution
            rules.flatMap { rule =>
                val renamedRule = renameVariables(rule) // Rename variables in the rule
                Unification.unify(goal, renamedRule.head, subst) match {
                    case Some(unifiedSubst) =>
                        resolveAll(renamedRule.body, rules, newVisited, unifiedSubst) // Explore body
                    case None =>
                        Nil // Unification failed
                }
            }
        }
    }

    def resolveAll(goals: List[Term], rules: List[Rule], visited: Set[(Term, Substitution)], subst: Substitution): List[Substitution] = {
        goals match {
            case Nil =>
                List(subst) // All goals resolved successfully
            case Compound("not", List(term)) :: tail =>
                if (not(term, rules, subst)) {
                    resolveAll(tail, rules, visited, subst) // Negation succeeded
                } else {
                    Nil // Negation failed
                }
            case head :: tail =>
                resolve(head, rules, visited, subst).flatMap(newSubst =>
                    resolveAll(tail, rules, visited, newSubst) // Continue resolving remaining goals
                )
        }
    }
    
    def not(goal: Term, rules: List[Rule], subst: Substitution): Boolean = {
        goal match {
            case compoundGoal: Compound =>
                // Evaluate the goal and negate its result
                val results = resolve(compoundGoal, rules, Set.empty, subst)
                results.isEmpty // Negation succeeds if no results exist
            case _ =>
                throw new IllegalArgumentException(s"Negation is only supported for Compound terms, found: $goal")
        }
    }


    

    
    def forwardChain(rules: List[Rule]): Set[Term] = {
        var facts: Set[Term] = rules.filter(_.body.isEmpty).map(_.head).toSet
        var newFacts: Set[Term] = Set.empty

        var keepRunning = true
        while (keepRunning) {
        newFacts = rules.flatMap {
            case Rule(head, body) if body.forall(facts.contains) => Some(head)
            case _ => None
        }.toSet -- facts

        if (newFacts.isEmpty) keepRunning = false
        facts ++= newFacts
        }

        facts
    }

    def renameVariables(rule: Rule): Rule = {
        val renaming = scala.collection.mutable.Map[Variable, Variable]()
        def rename(term: Term): Term = term match {
            case v: Variable =>
                renaming.getOrElseUpdate(v, {
                    variableCounter += 1
                    Variable(s"${v.name}_${variableCounter}")
                })
            case Compound(functor, args) =>
                Compound(functor, args.map(rename))
            case other => other
        }

        Rule(rename(rule.head).asInstanceOf[Compound], rule.body.map(rename))
    }
}


@main def testBackWardChaining() = {
    val visited: Set[Substitution] =  Set.empty[Substitution]
    import RulesEngine._
    
    val query = Compound("bachelor", List(Variable("B")))
    val results = resolve(query, rules, Set.empty, Map.empty)

    println("Results========bachelor===================")
    results.map(r => println(s"results $r"))
    println("Results===========================")

    
    println("Testing Who are husbands")
    println("==========================================")
    resolve(Compound("husband", List(Variable("H"),Variable("W"))),rules,Set.empty, Map.empty ).foreach { result =>
        println(s"Result husbands ---------------: $result")
    }
    println("==========================================")

    
    println("Testing Who is husband of paulina: should be paul")
    println("==========================================")
    resolve(Compound("husband", List(Variable("H"),Atom("paulina"))),rules,Set.empty, Map.empty ).foreach { result =>
        println(s"Result husband of paulina ---------------: $result")
    }
    println("==========================================")

    
    println("Testing Who is mother of jenny: should be jen")
    println("==========================================")
    resolve(Compound("mother", List(Variable("M"),Atom("jenny"))),rules,Set.empty, Map.empty).foreach { result =>
        println(s"Result mother ---------------: $result")
    }
    println("==========================================")
    
    
    println("Testing Who is grandmother jenny should be granny")
    println("==========================================")
    resolve(Compound("grandmother", List(Variable("GM"),Atom("jenny"))),rules,Set.empty, Map.empty).foreach { result =>
        println(s"Result grandmother ---------------: $result")
    }
    println("==========================================")
        
}

@main def testForwardChaining =
    import RulesEngine._

    val forwardFacts = forwardChain(rules)
    println("Forward Chaining Facts:")
    forwardFacts.foreach(println)
    