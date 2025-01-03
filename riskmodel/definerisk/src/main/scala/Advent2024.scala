package com.advent2024

import scala.Iterator
import scala.collection.immutable.Queue

def parseDay2(input: String): Seq[Array[Long]] =
    val reports = input.stripMargin.linesIterator.toList
                    .map(line => line.split(" ").map(_.toLong))
                    .toSeq
    
    reports

def parse(input: String): (Seq[Long],Seq[Long]) =
    val pairs = input.stripMargin.linesIterator.toList
                    .map(line => line.split("   ").map(_.toLong))
                    .toSeq
    val left = pairs.map(_.head).toSeq.sorted
    val right = pairs.map(_.last).toSeq.sorted

    (left,right)

def total_distance(left :Seq[Long],right:Seq[Long]) : Long = {
    left.zip(right).map((l, r) => math.abs(l - r)).sum
    
}   

def similarityScore(left :Seq[Long],right:Seq[Long]) : Long = {
    left.map(l => right.count(_ == l) * l).sum
}

def isSafe(reportLine : Array[Long]): String = 

    val levelDiff = reportLine.init.zip(reportLine.tail) // learned this method init creates new array without the last, tail with the first

    val decreasing = levelDiff.forall(_ < _)
    val increasing = levelDiff.forall(_ > _)
    val adjLevelBetween1and3 =  levelDiff.forall( (a,b) => Math.abs(a-b) >= 1 && Math.abs(a-b) <= 3)
    
    val safe = (decreasing || increasing) && (adjLevelBetween1and3)
    
    if safe then "Safe" else "UnSafe"
    
def isSafeRemove1(reportLine : Array[Long]): String =
    val remove1Safe = (0 until reportLine.length).exists { index =>
        val newLevels = reportLine.take(index) ++ reportLine.drop(index + 1) // takes first index element, drop first index+1 elements index is removed
        isSafe(newLevels) == "Safe"
    }
     if remove1Safe then "SafeRemove1" else "Remove1UnSafe"

def getAllMul(input: String): Long =
    val mulPattern = """mul\((\d+),(\d+)\)""".r // .r converts the string to regex
    val muls = mulPattern.findAllIn(input)
    muls.collect {
        case mulPattern(a,b) => a.toInt * b.toInt // learned this from scala advent of code
    }.sum
    

def getAllMulDoAndDont(input: String): Long =
    val mulPattern = """mul\((\d+),(\d+)\)""".r // .r converts the string to regex REUSE this pattern from part1 !
    val allPattern = """(mul\((\d+),(\d+)\)|do\(\)|don't\(\))""".r // .r converts the string to regex
                     
    val (_,res) = allPattern.findAllIn(input).foldLeft((true,0)) {
        case ((true,sum),mulPattern(a,b)) => (true,sum + a.toInt * b.toInt) // learned this from scala advent of code
        case ((_,sum),"don't()") => (false,sum)
        case ((_,sum),"do()") => (true,sum)
        case ((flag,sum),_ ) => (flag,sum)
    }
    res

type Grid = Array[Array[Char]]

// scala advent of code
def parseDay4(input: String): Grid =
  Array.from(
    input.linesIterator.map(Array.from)
  )

case class Dir(dy: Int, dx: Int)
type Occurrence = (Int, Int, Dir)

val dirs = IArray(
  Dir(dy = -1, dx = 0), // up
  Dir(dy = 0, dx = 1), // right
  Dir(dy = 1, dx = 0), // down
  Dir(dy = 0, dx = -1), // left
  Dir(dy = -1, dx = 1), // up-right
  Dir(dy = 1, dx = 1), // down-right
  Dir(dy = 1, dx = -1), // down-left
  Dir(dy = -1, dx = -1) // up-left
)

def checkDirection(grid: Grid, word: String, row: Int, col: Int, dir: Dir): Boolean =
    
    word.indices.forall { i =>
      val newRow = row + i * dir.dx
      val newCol = col + i * dir.dy
      newRow >= 0 && newRow < grid.length && // Check row bounds
      newCol >= 0 && newCol < grid(newRow).length && // Check column bounds
      grid(newRow)(newCol) == word(i) // Check character match
    }

  // Search for all occurrences of the word and return their positions and directions
def findWordOccurrences(grid: Grid, word: String): List[Occurrence] =
    (for
      row <- grid.indices
      col <- grid(row).indices
      dir <- dirs if checkDirection(grid, word, row, col, dir)
    yield (row, col, dir))
    .toList

def highlightOccurrences(grid: Grid, word: String, occurrences: List[Occurrence]): Grid =
    val highlightedGrid =  grid.map(_.clone().map(_.toLower)) // Create a copy of the grid to modify
    occurrences.foreach { case (row, col, dir) =>
      for i <- word.indices do
        val newRow = row + i * dir.dx
        val newCol = col + i * dir.dy
        highlightedGrid(newRow)(newCol) = highlightedGrid(newRow)(newCol).toUpper // Mark character
    }
    highlightedGrid

  // Render the grid as a string
def renderGrid(grid: Grid): String =
    grid.map(_.mkString(" ")).mkString("\n")

def boundCheck(x: Int, y: Int, grid: Grid): Boolean =
    x >= 0 && x < grid.length && y >= 0 && y < grid(0).length

def scanner(x: Int, y: Int, dir: Dir, grid: Grid): Iterator[Char] =
    Iterator.unfold((y, x)): (y, x) =>
        Option.when(boundCheck(x, y, grid))(grid(y)(x) -> (y + dir.dy, x + dir.dx))

def scanString(target: String)(x: Int, y: Int, dir: Dir, grid: Grid): Boolean =
    scanner(x, y, dir, grid).take(target.length).corresponds(target)(_ == _)

val searchWord = "XMAS"
val scanXMAS = scanString(searchWord)

def totalXMAS(grid: Grid): Int =
  Iterator
    .tabulate(grid.size, grid.size): (y, x) =>
      dirs.count(dir => scanXMAS(x, y, dir, grid))
    .flatten
    .sum


@main def mainDay5() =     
    val rulesInput = 
     """*47|53
        *97|13
        *97|61
        *97|47
        *75|29
        *61|13
        *75|53
        *29|13
        *97|29
        *53|29
        *61|53
        *97|53
        *61|29
        *47|13
        *75|47
        *97|75
        *47|61
        *75|61
        *47|29
        *75|13
        *53|13"""

    val updatesInput = 
     """|75,47,61,53,29
        |97,61,53,29,13
        |75,29,13
        |75,97,47,61,53
        |61,13,29
        |97,13,75,29,47"""
    
    val rules = rulesInput.stripMargin('*')
        .linesIterator.toList.map { 
            _.split('|').map(_.toLong)
        }.map { 
            p => p(0) -> p(1)
        }.groupMap(_._1)(_._2) 
    
    
    //rules.toList.foreach(println)
    val updates = updatesInput.stripMargin.linesIterator.map( s => s.split(",").map(x => x.toLong).toList).toList
    //updates.toList.foreach(println)
    
    def isValid(rules: Map[Long,List[Long]])(update: List[Long]): Boolean = 
        def rec(update: List[Long], visited: Set[Long] = Set.empty): Boolean = 
            update match 
                case Nil => true
                case head :: tail => 
                    !rules.getOrElse(head,List.empty).exists( p => visited.contains(p))
                    && rec(tail,visited + head)
        
        rec(update)
    
    val midDigitSum = updates.filter(update => isValid(rules)(update)).map(up => up(up.size/2)).sum
    println(s"mid digit sum $midDigitSum")

    val invalidUpdates = updates.filter(!isValid(rules)(_))

    def fixUpdate(update: List[Long]): List[Long] =
        // filter rules that have k in the updates and there exists vs in the update
        // i.e. get numbers that are in the updates in k or vs of the rules
        // ignore numbers non in updates!
        println(s"update $update")
        val relevantRules = rules
            .filter((k, vs) => update.contains(k) && vs.exists(p => update.contains(p)))
            .mapValues(_.filter(update.contains)).toMap

        println(s"relevantRules $relevantRules")
        val prevsMap = relevantRules
            .map { case (k, vs) => vs.map(_ -> k) }
            .flatten.groupMap(_._1)(_._2)
        
        println(s"prevsMap $prevsMap")

        val startNodes = update.filter(k => !relevantRules.values.flatten.toList.contains(k))
        println(s"startNodes $startNodes")

        def bfs(queue: Queue[Long], visited: Set[Long] = Set.empty, res: List[Long] = List.empty): List[Long] = queue.dequeueOption match
            case None => res
            case Some((node, queue1)) => {
                val newVisited = visited + node
                val newRes = res :+ node
                val newQueue = relevantRules.getOrElse(node, List.empty)
                    .filter { n =>
                        val notVisited = !newVisited.contains(n)
                        val notInQueue = !queue1.contains(n)
                        val allPrevVisited = prevsMap.getOrElse(n, List.empty).forall(p => newVisited.contains(p) || queue1.contains(p))
                        notVisited && notInQueue && allPrevVisited
                    }
                    .foldLeft(queue1)(_.appended(_))
                bfs(newQueue, newVisited, newRes)
            }
        bfs(Queue.from(startNodes))
    //invalidUpdates.foreach(println)
    val part2Sum = invalidUpdates.map(fixUpdate).map(us => us(us.size / 2)).sum    
    println(s"mid digit sum $part2Sum")

@main def mainDay4ScalaAdventOfCode() = 
    val input = 
     """|MMMSXXMASM
        |MSAMXMSMSA
        |AMXSXMAAMM
        |MSAMASMSMX
        |XMASAMXAMM
        |XXAMMXXAMA
        |SMSMSASXSS
        |SAXAMASAAA
        |MAMMMXMMMM
        |MXMXAXMASX"""

    val raggedInput = 
     """|MMMSXXMASM
        |MSAMXMSMSAFDSFSFDSFDSFDSFDS
        |AMXSXMAAMMFDSFDSF
        |MSAMASMSMXFDSFDSFDSF
        |XMASAMXAMM45RREFESR
        |XXAMMXXAMA232
        |SMSMSASXSSWW2
        |SAXAMASAAA
        |MAMMMXMMMMAWDW
        |MXMXAXMASX"""
        
    {   // uniformed grid
        val reports = input.stripMargin.toString()
        println(reports)                    
        val grid = parseDay4(reports)
        val found = totalXMAS(grid)
        println(s"Found XMAS $found")
    }
    
    {   // ragged
        val reports = raggedInput.stripMargin.toString()
        println(reports)                    
        val grid = parseDay4(reports)

        val occurrences = findWordOccurrences(grid, searchWord)
        println(s"Word '$searchWord' found at positions: ${occurrences.length}")

        println(s"grid size ${grid.size} length ${grid.length} breath ${grid(0).length}")
        val highlightedGrid = highlightOccurrences(grid, searchWord, occurrences)
        println("\nHighlighted Grid:")
        println(renderGrid(highlightedGrid))
    }
    
    

@main def mainDay3() =     
    val input1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    val sum = getAllMul(input1)
    println(s"#1 sum of all mul $sum")

    val input2 = "xmul(2,4)%&mul[3,7]!@^don't()_mul(5,5)+mul(32,64]then(mul(11,8)undo()mul(8,5))"
    val sum2 = getAllMulDoAndDont(input2)
    println(s"#2 sum of all mul $sum2")

@main def mainDay2() = 
    val inputDay2 = 
    """|7 6 4 2 1
       |1 2 7 8 9
       |9 7 6 2 1
       |1 3 2 4 5
       |8 6 4 4 1
       |1 3 6 7 9"""

    val reports = parseDay2(inputDay2)
    
    val result = reports.foldLeft(Seq.empty[(Array[Long], String,String)]) {
        (acc, elem) =>  acc :+ (elem,isSafe(elem),isSafeRemove1(elem)) // :+ keeps the order
    } 
    result.foreach { (sq,s,r1s) =>
        println(s"${sq.mkString(", ")} $s $r1s")
    }
    val safeCount = result.count(p => p._2 == "Safe")
    println(s"Total Safe $safeCount")

@main def mainDay1() =
    val inputDay1 = 
    """ |3   4
        |4   3
        |2   5
        |1   3
        |3   9
        |3   3"""
    val pairs = parse(inputDay1)
    println("---------------------------")
    pairs._1.foreach(num => println(num))
    println("---------------------------")
    pairs._2.foreach(num => println(num))
    val distance = total_distance(pairs._1, pairs._2)
    println(s"Total Distance $distance")
    val similar =  similarityScore(pairs._1, pairs._2)
    println(s"Total Similar $similar")