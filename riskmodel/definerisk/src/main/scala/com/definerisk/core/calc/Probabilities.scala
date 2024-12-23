package com.definerisk.core.calc

import scala.util.Random

// Core Models
enum Outcome:
  case Success, Failure

trait Event:
  def name: String
  def probability: Double

case class SimpleEvent(name: String, probability: Double) extends Event

trait Constraint:
  def apply(event: Event): Boolean

case class ProbabilityConstraint(min: Double, max: Double) extends Constraint:
  override def apply(event: Event): Boolean =
    event.probability >= min && event.probability <= max

// Probability Utilities
object ProbabilityUtils:
  def combination(n: Int, r: Int): Int =
    if r > n then 0 else (factorial(n) / (factorial(r) * factorial(n - r)))

  def permutation(n: Int, r: Int): Int =
    if r > n then 0 else factorial(n) / factorial(n - r)

  def factorial(n: Int): Int =
    if n == 0 then 1 else (1 to n).product

  //def factorial(n: Int): Long = if n <= 1 then 1 else n * factorial(n - 1)

  def combinations(n: Int, r: Int): Long =
    require(n >= r && r >= 0, "Invalid combination inputs")
    factorial(n) / (factorial(r) * factorial(n - r))

  def mean(data: Seq[Double]): Double = data.sum / data.size

  def probabilityOfEvent(successes: Int, trials: Int): Double =
    successes.toDouble / trials

// Simulation Engine
case class SimulationResult(eventName: String, outcome: Outcome)

class ProbabilitySimulation(events: List[Event], constraints: List[Constraint]):
  private val random = Random()

  def runSimulation(trials: Int): List[SimulationResult] =
    (1 to trials).toList.flatMap { _ =>
      events.filter(event => constraints.forall(_.apply(event))).map { event =>
        val outcome = if random.nextDouble() <= event.probability then Outcome.Success else Outcome.Failure
        SimulationResult(event.name, outcome)
      }
    }

  def tabulateResults(results: List[SimulationResult]): Map[String, (Int, Int)] =
    results.groupBy(_.eventName).view.mapValues { events =>
      val successes = events.count(_.outcome == Outcome.Success)
      val failures = events.size - successes
      (successes, failures)
    }.toMap

  def firstSuccess(results: List[SimulationResult]): Option[SimulationResult] =
    results.find(_.outcome == Outcome.Success)

  def lastSuccess(results: List[SimulationResult]): Option[SimulationResult] =
    results.reverse.find(_.outcome == Outcome.Success)

object SockDrawer:
  def probabilityTwoRed(R: Int, B: Int): Double =
    if R + B < 2 then 0.0 // Not enough socks
    else (R.toDouble * (R - 1)) / ((R + B) * (R + B - 1))

  def findSmallestSocks(targetProbability: Double, evenBlack: Boolean = false): (Int, Int) =
    var minSocks = Int.MaxValue
    var result = (0, 0)

    for r <- 1 to 100 do
      for b <- 0 to 100 do
        if !evenBlack || b % 2 == 0 then
          val prob = probabilityTwoRed(r, b)
          if prob == targetProbability && (r + b) < minSocks then
            minSocks = r + b
            result = (r, b)

    result

object ElmerGame:
  // Compute probability of winning a specific series
  def probabilityOfWinningSeries(series: List[Double]): Double =
    val n = series.size
    val outcomes = generateOutcomes(n) // Generate all possible outcomes (2^n)
    val winningOutcomes = outcomes.filter(outcome => hasConsecutiveWins(outcome))

    winningOutcomes.map(outcomeProbability(_, series)).sum

  // Generate all possible outcomes for n games (e.g., [W, L, W])
  private def generateOutcomes(n: Int): List[List[Boolean]] =
    (0 until (1 << n)).toList.map { mask =>
      (0 until n).map(i => (mask & (1 << i)) != 0).toList
    }

  // Check if an outcome has at least 2 consecutive wins
  private def hasConsecutiveWins(outcome: List[Boolean]): Boolean =
    outcome.sliding(2).exists(window => window.forall(identity))

  // Compute the probability of a specific outcome
  private def outcomeProbability(outcome: List[Boolean], probabilities: List[Double]): Double =
    outcome.zip(probabilities).map {
      case (true, p)  => p   // Win: use the probability of winning
      case (false, p) => 1 - p // Loss: use the probability of losing
    }.product

object ProbabilityFramework:

  /** Represents a probability model */
  case class Probability(p: Double):
    require(p >= 0.0 && p <= 1.0, "Probability must be between 0 and 1")

    /** Complement of this probability */
    def complement: Probability = Probability(1.0 - p)

    /** Combine probabilities with AND */
    def and(that: Probability): Probability = Probability(this.p * that.p)

    /** Combine probabilities with OR (assuming independence) */
    def or(that: Probability): Probability = 
      Probability(this.p + that.p - this.and(that).p)

  /** Represents an event with a name and a probability */
  case class Event(name: String, prob: Probability)

  /** Utility object for probability functions */
  object ProbabilityUtils:
    
    /** Compute the probability of at least k successes in n trials */
    def binomialProbability(n: Int, k: Int, p: Probability): Double =
      val coeff = factorial(n) / (factorial(k) * factorial(n - k))
      coeff * Math.pow(p.p, k) * Math.pow(p.complement.p, n - k)

    /** Compute factorial */
    def factorial(n: Int): Long = if n == 0 then 1 else n * factorial(n - 1)

  /** Solve the jury problem */
  object JuryProblemSolver:
    
    /** Probability of 3-man jury making the correct decision */
    def threeManJury(p: Probability): Probability =
      val pCoinFlip = Probability(0.5) // Coin flip for third member
      val allCorrect = p.and(p).and(pCoinFlip)
      val twoCorrect = ProbabilityUtils.binomialProbability(3, 2, p).toDouble
      Probability(allCorrect.p + twoCorrect)

    /** Probability of 1-man jury making the correct decision */
    def oneManJury(p: Probability): Probability = p

    /** Compare the two juries */
    def compareJuries(p: Probability): String =
      val threeMan = threeManJury(p)
      val oneMan = oneManJury(p)
      if threeMan.p > oneMan.p then
        f"Three-man jury is better with probability: ${threeMan.p}%.4f"
      else
        f"One-man jury is better with probability: ${oneMan.p}%.4f"

object DiceSimulation:

  /** Simulates the number of throws needed to roll a target number on a die */
  def throwsToGetTarget(sides: Int, target: Int): Int =
    require(sides > 0, "A die must have at least one side")
    require(target > 0 && target <= sides, s"Target must be between 1 and $sides")

    @annotation.tailrec
    def rollUntilTarget(count: Int = 0): Int =
      val roll = scala.util.Random.between(1, sides + 1)
      if roll == target then count + 1
      else rollUntilTarget(count + 1)

    rollUntilTarget()

  /** Computes the average number of throws to get a target number */
  def averageThrowsToGetTarget(sides: Int, target: Int, simulations: Int): Double =
    require(simulations > 0, "Number of simulations must be positive")

    val totalThrows = (1 to simulations).map(_ => throwsToGetTarget(sides, target)).sum
    totalThrows.toDouble / simulations

object GamblersRuin:

  case class GameOutcome(n: Double, m: Double, ruinProbabilities: (Double, Double))

  /** Calculate ruin probabilities for Sam and John */
  def calculateRuinProbabilities(n: Double, m: Double): GameOutcome =
    require(n > 0, "n (money ratio) must be positive")
    require(m > 0, "m (probability ratio) must be positive")

    if m == 1 then
      // Fair game case
      GameOutcome(n, m, (0.0, 1.0))
    else
      val pJohn = (1 - Math.pow(m, n)) / (1 - Math.pow(m, n + 1))
      val pSam = 1 - pJohn
      GameOutcome(n, m, (pSam, pJohn))

  @main def runGamblersRuin(): Unit =
    val n = 2.0  // Sam's money is twice John's money
    val m = 2.0  // John is twice as likely to win a game

    val outcome = calculateRuinProbabilities(n, m)
    println(f"Money ratio (n): ${outcome.n}")
    println(f"Probability ratio (m): ${outcome.m}")
    println(f"Sam's probability of ruin: ${outcome.ruinProbabilities._1}%.4f")
    println(f"John's probability of ruin: ${outcome.ruinProbabilities._2}%.4f")


@main def runDiceSimulation(): Unit =
  val sides = 6
  val target = 6
  val simulations = 100000

  val averageThrows = DiceSimulation.averageThrowsToGetTarget(sides, target, simulations)
  println(f"Average throws needed to roll a $target: $averageThrows%.4f")


@main def runJuryComparison(): Unit =
  import ProbabilityFramework.*
  
  // Define the probability of a jury member making the correct decision
  val p = Probability(0.7)

  // Solve the jury problem
  val result = JuryProblemSolver.compareJuries(p)
  println(result)


@main def solveElmerGame(): Unit =
  val elmerVsJohn = 0.7  // Elmer's probability of winning against John
  val elmerVsSam = 0.4   // Elmer's probability of winning against Sam

  // Series 1: john-sam-john
  val series1 = List(elmerVsJohn, elmerVsSam, elmerVsJohn)
  val probSeries1 = ElmerGame.probabilityOfWinningSeries(series1)

  // Series 2: sam-john-sam
  val series2 = List(elmerVsSam, elmerVsJohn, elmerVsSam)
  val probSeries2 = ElmerGame.probabilityOfWinningSeries(series2)

  println(f"Probability of winning john-sam-john: $probSeries1%.4f")
  println(f"Probability of winning sam-john-sam: $probSeries2%.4f")

  if probSeries1 > probSeries2 then
    println("Elmer should choose the series john-sam-john.")
  else
    println("Elmer should choose the series sam-john-sam.")


@main def solveSockDrawerProblem(): Unit =
  // Part a: Smallest number of socks
  val (r1, b1) = SockDrawer.findSmallestSocks(targetProbability = 0.5)
  println(s"Part a: Smallest number of socks: R = $r1, B = $b1, Total = ${r1 + b1}")

  // Part b: Smallest number with even black socks
  val (r2, b2) = SockDrawer.findSmallestSocks(targetProbability = 0.5, evenBlack = true)
  println(s"Part b: Smallest number of socks with even B: R = $r2, B = $b2, Total = ${r2 + b2}")

@main def runSimulation(): Unit =
  // Define events
  val redSocks = SimpleEvent("Draw Two Red Socks", 0.5)
  val blackSocks = SimpleEvent("Draw Two Black Socks", 0.25)
  val mixedSocks = SimpleEvent("Draw One Red and One Black Sock", 0.75)

  // Define constraints
  val constraints = List(ProbabilityConstraint(0.2, 0.8))

  // Create simulation
  val simulation = ProbabilitySimulation(List(redSocks, blackSocks, mixedSocks), constraints)

  // Run simulation
  val trials = 10000
  val results = simulation.runSimulation(trials)

  // Tabulate results
  val tabulated = simulation.tabulateResults(results)
  println("Results Table:")
  tabulated.foreach { case (eventName, (successes, failures)) =>
    println(s"$eventName: Successes = $successes, Failures = $failures")
  }

  // First and last success
  val first = simulation.firstSuccess(results)
  val last = simulation.lastSuccess(results)

  println(s"First Success: ${first.map(_.eventName).getOrElse("None")}")
  println(s"Last Success: ${last.map(_.eventName).getOrElse("None")}")

import scala.annotation.tailrec
import scala.util.Random

// Core Trait for any probability problem
trait ProbabilityProblem:
  def name: String
  def description: String
  def solveTheoretically(): Option[Double]
  def simulate(trials: Int): Double

// Utility object for combinatorics and probability-related operations

// Framework for solving problems
object ProblemSolver:
  def solve(problem: ProbabilityProblem, trials: Int = 100000): Unit =
    println(s"Solving: ${problem.name}")
    println(s"Description: ${problem.description}")

    // Solve theoretically if possible
    problem.solveTheoretically() match
      case Some(result) =>
        println(f"Theoretical solution: $result%.4f")
      case None =>
        println("No theoretical solution provided")

    // Solve using simulation
    val simulatedResult = problem.simulate(trials)
    println(f"Simulated solution (after $trials trials): $simulatedResult%.4f")
    println("=====================================")


// Problem 1: Dice Throw Problem
case class DiceThrow(sides: Int, target: Int) extends ProbabilityProblem:
  def name: String = "Dice Throw Problem"
  def description: String = s"Find average throws needed to get $target on a $sides-sided die."

  def solveTheoretically(): Option[Double] =
    Some(sides.toDouble)

  def simulate(trials: Int): Double =
    val totalThrows = (1 to trials).map { _ =>
      @tailrec
      def roll(count: Int = 0): Int =
        val result = Random.between(1, sides + 1)
        if result == target then count + 1 else roll(count + 1)
      roll()
    }
    ProbabilityUtils.mean(totalThrows.map(_.toDouble))

// Problem 2: Gambler's Ruin
case class GamblersRuin(n: Double, m: Double) extends ProbabilityProblem:
  def name: String = "Gambler's Ruin Problem"
  def description: String = s"Find ruin probabilities for Sam (n = $n) and John (m = $m)."

  def solveTheoretically(): Option[Double] =
    if m == 1 then Some(1.0) // Fair game
    else Some(1 - Math.pow(m, n) / (1 - Math.pow(m, n + 1)))

  def simulate(trials: Int): Double =
    val totalWins = (1 to trials).count { _ =>
      @tailrec
      def playGame(samMoney: Double, johnMoney: Double): Boolean =
        if samMoney <= 0 then false
        else if johnMoney <= 0 then true
        else
          val samWins = Random.nextDouble() < (1.0 / m)
          if samWins then playGame(samMoney, johnMoney - 1)
          else playGame(samMoney - 1, johnMoney)
      playGame(n, 1)
    }
    ProbabilityUtils.probabilityOfEvent(totalWins, trials)

// Main App
@main def runFramework(): Unit =
  val diceProblem = DiceThrow(sides = 6, target = 6)
  val gamblersRuin = GamblersRuin(n = 2.0, m = 2.0)

  ProblemSolver.solve(diceProblem)
  ProblemSolver.solve(gamblersRuin)
