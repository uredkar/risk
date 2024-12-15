import com.definerisk.core.models.{*,given }

class RulesEngine {
    var facts: Set[Term] = Set.empty // Stores known facts
    var rules: List[Rule] = List(
        Rule(Compound("male", List(Atom("john"))), Nil),
        Rule(Compound("male", List(Atom("peter"))), Nil),
        Rule(Compound("male", List(Atom("paul"))), Nil),
        Rule(Compound("married", List(Atom("paul"),Atom("paulina"))), Nil),
        
        
        Rule(Compound("husband", List(Variable("X"),Variable("Y"))), List(
            Compound("male", List(Variable("X"))),
            Compound("married",List(Variable("X"),(Variable("Y"))))
        ))
    )

    // Add a fact to the engine
    def addFact(fact: Term): Unit = {
        facts += fact
    }

    // Add a rule to the engine
    def addRule(rule: Rule): Unit = {
        rules = rules :+ rule
    }

    def substitute(term: Term, subst: Substitution): Term = term match {
        case v: Variable => subst.getOrElse(v, v)
        case Compound(f, args) => Compound(f, args.map(arg => substitute(arg, subst)))
        case _ => term
    }

    def unify(x: Term, y: Term, subst: Substitution): Option[Substitution] = 
        
        val ns = (x, y) match {
            case (a1: Atom, a2: Atom) if a1 == a2 => Some(subst)
            case (v: Variable, t) => unifyVariable(v, t, subst)
            case (t, v: Variable) => unifyVariable(v, t, subst)
            case (Compound(f1, args1), Compound(f2, args2)) if f1 == f2 =>
                unifyArgs(args1, args2, subst)
            case _ => None
        }
        //println(s"unify $x <-> $y  subst $subst ns $ns")
        ns

    def unifyVariable(v: Variable, t: Term, subst: Substitution): Option[Substitution] = {
        if (t == v) Some(subst)
        else if (occursCheck(v, t, subst)) None
        else Some(subst + (v -> t))
    }

    def unifyArgs(args1: List[Term], args2: List[Term], subst: Substitution): Option[Substitution] = {
        (args1, args2) match {
        case (Nil, Nil) => Some(subst)
        case (a1 :: rest1, a2 :: rest2) =>
            unify(a1, a2, subst).flatMap(unifyArgs(rest1, rest2, _))
        case _ => None
        }
    }

    def occursCheck(v: Variable, t: Term, subst: Substitution): Boolean = {
        def occurs(term: Term): Boolean = term match {
            case `v` => true
            case Compound(_, args) => args.exists(occurs)
            case _ => false
        }
        occurs(substitute(t, subst))
    }
    // Resolve a goal against facts and rules
    def resolve(goal: Term, subst: Substitution = Map(),visited: Set[Substitution]): List[Substitution] = {
        println(s"\n\nresolve $goal")
        //resolveFact(goal, subst) ::: 
        resolveRules(goal, subst,visited)
    }

    // Resolve a goal directly against known facts
    def resolveFact(goal: Term, subst: Substitution): List[Substitution] = {
        println("resolveFact")
        facts.collect {
            case fact if unify(goal, fact, subst).isDefined => unify(goal, fact, subst).get
        }.to(List)
    }

    // Resolve a goal using rules
    def resolveRules(goal: Term, subst: Substitution,visited: Set[Substitution]): List[Substitution] = {
        println(s"\tresolveRules goal $goal subst $subst")
        rules.to(List).flatMap {
            case Rule(head, body) =>
                println(s"\t->head $head")
                val ns = unify(goal, head, subst) 
                //println(s"resolveRules $ns")
                ns match {
                    case None =>
                        //println(s"Rule head does not unify: $head")
                        List.empty
                    case Some(newSubst) =>
                        val combinedSubst = newSubst ++ subst
                        println(s"\t->>>>>>Rule head unified: $head -> Subst: $newSubst")
                        if (!visited.contains(combinedSubst))
                            resolveGoals(body, newSubst,visited+newSubst)
                        else LazyList.empty
                }
        }
    }

    // Resolve a list of goals
    def resolveGoals(goals: List[Term], subst: Substitution,visited: Set[Substitution]): List[Substitution] = 
        println(s"\t\tResolving goals: $goals under substitution $subst")
        goals match {
            case Nil => List(subst)
            case Cut :: _ => List(subst) // Stop backtracking when encountering a cut
            case Not(goal) :: rest =>
                println(s"\t\tnot $goal")
                resolve(goal, subst,visited+subst) match {
                    case List() => 
                        println("\t\tgoal cannot be proven")
                        resolveGoals(rest, subst,visited+subst) // `goal` cannot be proven, continue
                    case other => 
                        println(s"\n\t\tNot Fails Success $other")
                        List.empty // `goal` is proven, `not` fails
                }
            case goal :: rest =>
                //resolve(goal, subst,visited).flatMap(s => resolveGoals(rest, s,visited))
                resolve(goal, subst, visited+subst).flatMap { newSubst =>
                    if (!visited.contains(newSubst))
                        resolveGoals(rest, newSubst, visited + newSubst)
                    else LazyList.empty
                }

    }

    // Perform forward chaining to deduce new facts
    def forwardChain(): Unit = {
        var newFacts = Set.empty[Term]
        var updated = true
        val visited: Set[Substitution] =  Set.empty[Substitution]
        while (updated) {
        updated = false
        for (Rule(head, body) <- rules) {
            resolveGoals(body, Map.empty,visited).foreach { subst =>
            val resolvedHead = substitute(head, subst)
            if (!facts.contains(resolvedHead)) {
                newFacts += resolvedHead
                updated = true
            }
            }
        }
        facts ++= newFacts
        }
    }
}

@main def TestSimpleRule = {
    { // simple
        val engine = new RulesEngine()
        val subst = Map.empty[Variable, Term]
        val result = engine.unify(Variable("X"), Atom("a"), subst)
        println(s"Result simple: $result") // Should output: Map(Variable("X") -> Atom("a"))
    }
    { // compound
        val engine = new RulesEngine()
        val subst = Map.empty[Variable, Term]
        val term1 = Compound("parent", List(Variable("X"), Atom("b")))
        val term2 = Compound("parent", List(Atom("a"), Atom("b")))
        val result = engine.unify(term1, term2, subst)
        println(s"Result Compound: $result") // Should output: Map(Variable("X") -> Atom("a"))
    }
    
}

@main def TestComplexRule() = {
    val visited: Set[Substitution] =  Set.empty[Substitution]
    val engine = new RulesEngine()
    /*
    // Add facts
    engine.addFact(Atom("rain"))
    engine.addFact(Compound("outside", List(Atom("john"))))

    // Add rules
    engine.addRule(Rule(Compound("wet", List(Atom("john"))), List(Atom("rain"), Compound("outside", List(Atom("john"))))))
    engine.addRule(Rule(Compound("dry", List(Atom("john"))), List(Not(Atom("rain")))))

    // Test backward chaining
    println("==========================================")
    println("Testing wet(john):")
    engine.resolve(Compound("wet", List(Variable("X")))).foreach { result =>
        println(s"\n\nResult Wet---------------: $result")
    }
    println("==========================================")
    
    println("==========================================")
    println("Testing dry(john):")
    engine.resolve(Compound("dry", List(Variable("X")))).foreach { result =>
        println(s"\n\nResult Should not see this -----------------------: $result")
    }
    println("==========================================")
    */  
    //val query3 = Compound("married", List(Atom("jim")))
    println("==========================================")
    println("Testing Who is husband:")
    engine.resolve(Compound("husband", List(Variable("X"),Variable("paulina"))),visited = visited).foreach { result =>
        println(s"\n\nResult husband ---------------: $result")
    }
    println("==========================================")
        
}
object TestRuleEngine {
    @main def main = {
        def forward1() = {
            val engine = new RulesEngine

            // Facts
            engine.addFact(Atom("rain"))
            engine.addFact(Compound("hasUmbrella", List(Atom("john"))))
            engine.addFact(Atom("storm"))

            // Rules
            //engine.addRule(Rule(Compound("wet", List(Atom("john"))), List(Atom("rain"), Compound("outside", List(Atom("john"))))))
            engine.addRule(Rule(Compound("wet", List(Atom("john"))), List(Atom("rain"), Compound("outside", List(Atom("john"))))))

            engine.addRule(Rule(Compound("safe", List(Atom("john"))), List(Compound("hasUmbrella", List(Atom("john"))))))
            engine.addRule(Rule(Compound("outside", List(Atom("john"))), List(Atom("rain"))))
            engine.addRule(Rule(Compound("dry", List(Atom("john"))), List(Not(Atom("rain")))))

            // Forward Chaining
            println("=== Forward Chaining ===")
            engine.forwardChain()
            println("Facts after forward chaining:")
            engine.facts.foreach(println)
        }

        

        def negation() = {
            val visited: Set[Substitution] = Set.empty[Substitution]
            val engine = new RulesEngine()
            // Negative Goal
            println("\n=== Negation ===")
            val negGoal = Compound("dry", List(Atom("john")))
            if (engine.resolve(negGoal,visited = visited).isEmpty) {
                println(s"$negGoal cannot be proven")
            }
        }

        def moreback() = {
            val visited: Set[Substitution] = Set.empty[Substitution]
            val engine = new RulesEngine()

            // Add facts
            engine.addFact(Atom("rain"))
            engine.addFact(Compound("outside", List(Atom("john"))))

            // Add rules
            engine.addRule(Rule(Compound("wet", List(Atom("john"))), List(Atom("rain"), Compound("outside", List(Atom("john"))))))
            engine.addRule(Rule(Compound("dry", List(Atom("john"))), List(Not(Atom("rain")))))

            // Test backward chaining
            println("Testing wet(john):")
            engine.resolve(Compound("wet", List(Atom("john"))),visited = visited).foreach(println)

            println("Testing dry(john):")
            engine.resolve(Compound("dry", List(Atom("john"))),visited = visited).foreach(println)

        }

        
    }
}