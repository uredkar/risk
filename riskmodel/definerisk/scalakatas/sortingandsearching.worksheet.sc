import scala.Iterator
import scala.collection.immutable.Queue
val originalMap: Map[String, Int] = Map(
      "apple" -> 1,
      "banana" -> 2,
      "orange" -> 3
    )

// Using mapValues to double the values
originalMap.mapValues(_ * 2).toMap

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


import scala.collection.mutable
def printTree(root: Long, nodes: List[(Long, Long)]): Unit = {
    // Build a mutable map to efficiently map parent IDs to their children
    val children: mutable.Map[Long, List[Long]] = mutable.Map.empty

    // Populate the children map
    nodes.foreach { 
        case (parent, child) =>
            children(parent) = children.getOrElseUpdate(parent, Nil) :+ child
            
    }

    // Recursive function to print the tree
    def printSubtree(parent: Long, level: Int = 0): Unit = {
        children.get(parent) match {
        case Some(childrenList) =>
            childrenList.foreach { child =>
                val prefix = "   " * level + "|--"
                println(s"$prefix${child}")
                printSubtree(child, level + 1)
            }
        case None => // Do nothing for leaf nodes
        }
    }


    // Print the tree starting from the root (parent_id is 0)
    println(s"root $root")
    printSubtree(root) 
}        

 val nodes: List[(Long, Long)] = List(
      (0L, 1L),
      (0L, 2L),
      (1L, 3L),
      (1L, 4L),
      (2L, 5L),
      (2L, 6L)
    )

printTree(0,nodes)

val nodes2 = rulesInput.stripMargin('*')
    .linesIterator.toList.map { 
        _.split('|').map(_.toLong)
    }.map(p => p(0) -> p(1))

for update <- updates 
    up <- update do
        printTree(up,nodes2)


import scala.util.Random

def validateList(randomList: List[Int], unorderedTuples: List[(Int, Int)]): (Boolean, List[(Int, Int)]) = {
  val tupleValues = unorderedTuples.flatMap { case (a, b) => List(a, b) }.distinct
  val filteredList = randomList.filter(tupleValues.contains)

  val violations = unorderedTuples.filter { case (a, b) =>
    val idxA = filteredList.indexOf(a)
    val idxB = filteredList.indexOf(b)
    idxA != -1 && idxB != -1 && idxA > idxB
  }
  (violations.isEmpty, violations)
}



def fixListOrderByRemoving(randomList: List[Int], unorderedTuples: List[(Int, Int)]): (List[Int], List[Int]) = {
  val tupleValues = unorderedTuples.flatMap { case (a, b) => List(a, b) }.distinct
  val filteredList = randomList.filter(tupleValues.contains)

  // Track removed items
  var removedItems = List.empty[Int]

  // Iteratively drop items that violate the tuple order
  val fixedList = unorderedTuples.foldLeft(filteredList) { (acc, tuple) =>
    val (a, b) = tuple
    val idxA = acc.indexOf(a)
    val idxB = acc.indexOf(b)

    if idxA != -1 && idxB != -1 && idxA > idxB then
      // Remove b if it violates the order
      removedItems :+= b
      acc.filterNot(_ == b)
    else acc
  }
  (fixedList, randomList.diff(fixedList))
}

/** Function to validate and fix the list by reordering while respecting tuple constraints */
def fixAndValidateListByReOrdering(randomList: List[Int], unorderedTuples: List[(Int, Int)]): (Boolean, List[Int], List[(Int, Int)]) = {
  val tupleValues = unorderedTuples.flatMap { case (a, b) => List(a, b) }.distinct
  val filteredList = randomList.filter(tupleValues.contains)

  // Attempt to reorder the list while respecting tuple constraints
  val reordered = reorderRespectingTuples(filteredList, unorderedTuples)

  

  val (isValid, violations) = validateList(reordered, unorderedTuples)
  (violations.isEmpty, reordered, violations)
}



def reorderRespectingTuples(list: List[Int], unorderedTuples: List[(Int, Int)]): List[Int] = {
  
  var reordered = list
  var changesMade = true
  var iterations = 0

  while (changesMade && iterations < list.size) {
    changesMade = false
    iterations += 1

    for ((a, b) <- unorderedTuples) {
      val idxA = reordered.indexOf(a)
      val idxB = reordered.indexOf(b)

      if (idxA != -1 && idxB != -1 && idxA > idxB) {
        // Swap positions of a and b to fix the order
        val (before, rest) = reordered.splitAt(idxA.min(idxB))
        val (toFix, after) = rest.splitAt((idxA - idxB).abs + 1)

        reordered = before ++ (toFix.sortBy(x => if (x == a) 0 else if (x == b) 1 else Int.MaxValue)) ++ after
        changesMade = true
      }
    }
  }

  // Final validation to ensure no constraints are violated
  val (isValid, violations) = validateList(reordered, unorderedTuples)
  if isValid then reordered else list
  
}

val unorderedTuples1 = rulesInput.stripMargin('*')
.linesIterator.toList.map { 
    _.split('|').map(_.toInt)
}.map { 
    p => (p(0),p(1))
}

val updates1 = updatesInput.stripMargin.linesIterator.map( s => s.split(",").map(x => x.toInt).toList).toList

// Step 1: Generate a list of unordered tuples with unique items
val unorderedTuples = (1 to 100).grouped(2).map { group =>
    if group.size == 2 && Random.nextBoolean() then (group.last, group.head)
    else 
        (group.head, group.last)

}.toList

println("Unordered Tuples:")
println(unorderedTuples)

// Step 2: Generate a random list of unique numbers
val randomNumbers = Random.shuffle(1 to 100).take(15).toList
println("Random List of Numbers:")
println(randomNumbers)

val updates2 = updatesInput.stripMargin.linesIterator.map( s => s.split(",").map(x => x.toInt).toList).toList
updates2.foreach{ update => 
    val (isValid, violations) = validateList(update, unorderedTuples1)
    violations.foreach { case (a, b) => println(s"Rule violated: $a -> $b") }
    if !isValid then
        println("*" * 10)
        println(s"Bad $update")
        
        val (fixedList, removedItems) = fixListOrderByRemoving(update, unorderedTuples1)
        println("Fixed List:")
        println(fixedList)
        println("Removed Items:")
        println(removedItems)
        
        val (isValid, reorderedList, violations) = fixAndValidateListByReOrdering(update, unorderedTuples1)

        println(s"Is the random list valid? $isValid")
        if !isValid then {
            println("Violations:")
            violations.foreach { case (a, b) => println(s"Rule violated: $a -> $b") }
        }

        println("Reordered List:")
        println(reorderedList)
    else 
        println(s"Good $update")

}
/*
 // Step 3: Validate the random list and fix if necessary
val (isValid, violations) = validateList(randomNumbers, unorderedTuples)
println(s"Is the random list valid? $isValid")
if !isValid then println("Violations:") else println("No Violations!")
violations.foreach { case (a, b) => println(s"Rule violated: $a -> $b") }

if !isValid then
    val (fixedList, removedItems) = fixListOrder(randomNumbers, unorderedTuples)
    
    println("Fixed List:")
    println(fixedList)
    println("Removed Items:")
    println(removedItems)
    val (sortedList, removedItems2) = sortOrFixList(randomNumbers, unorderedTuples)
    println("Sorted/Fully Fixed List:")
    println(sortedList)
    println("Removed Items (if applicable):")
    println(removedItems2)
*/