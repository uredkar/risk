sealed trait DoublyLinkedList[+A] {
    // Add a new value to the end of the list
    def add[B >: A](value: B): DoublyLinkedList[B] = this match {
        case Empty => Node(value, Empty, Empty)
        case Node(v, prev, Empty) => 
        val newNode = Node(value, this, Empty)
        Node(v, prev, newNode)
        case Node(v, prev, next) => 
        Node(v, prev, next.add(value))
    }

    // Delete the first node with the given value
    def delete[B >: A](value: B): DoublyLinkedList[A] = this match {
        case Empty => Empty
        case Node(v, prev, next) if v == value =>
        next match {
            case Node(nv, _, nnext) => Node(nv, prev, nnext)
            case Empty => prev
        }
        case Node(v, prev, next) =>
        Node(v, prev, next.delete(value))
    }

    // Modify the first node with the given value, replacing it with a new value
    def modify[B >: A](oldValue: B, newValue: B): DoublyLinkedList[B] = this match {
        case Empty => Empty
        case Node(v, prev, next) if v == oldValue =>
            Node(newValue, prev, next)
        case Node(v, prev, next) =>
            Node(v, prev, next.modify(oldValue, newValue))
        }

    def modifyWithPredicate[B >: A](predicate: B => Boolean, transform: B => B): DoublyLinkedList[B] = this match {
        case Empty => Empty
        case Node(v, prev, next) if predicate(v) =>
            Node(transform(v), prev, next)
        case Node(v, prev, next) =>
            Node(v, prev, next.modifyWithPredicate(predicate, transform))
       
        }    

    def deleteWithPredicate[B >: A](predicate: B => Boolean): DoublyLinkedList[B] =  this match {
        case Empty => Empty
        case Node(v, prev, next) if predicate(v) => delete(v)
        case Node(v, prev, next) =>
            Node(v, prev, next.deleteWithPredicate(predicate))
        }
    // Concatenate this list with another list
    def concat[B >: A](other: DoublyLinkedList[B]): DoublyLinkedList[B] = this match {
        case Empty => other
        case Node(v, prev, next) =>
            Node(v, prev, next.concat(other))
    }

    // FlatMap for backward traversal
    def flatMapBackward[B](f: A => DoublyLinkedList[B]): DoublyLinkedList[B] = this match {
        case Empty => Empty
        case Node(v, prev, _) =>
            prev.flatMapBackward(f).concat(f(v))
    }

  // FlatMap for forward traversal
    def flatMapForward[B](f: A => DoublyLinkedList[B]): DoublyLinkedList[B] = this match {
        case Empty => Empty
        case Node(v, _, next) =>
            f(v).concat(next.flatMapForward(f))
    }

    // Pretty-print the list
    def prettyPrint: String = {
        @annotation.tailrec
        def loop(curr: DoublyLinkedList[A], acc: String): String = curr match {
            case Empty => acc
            case Node(value, _, next) =>
            loop(next, if (acc.isEmpty) value.toString else s"$acc <-> $value")
        }
        loop(this, "")
    }
    
}


case object Empty extends DoublyLinkedList[Nothing] 

case class Node[A](value: A, prev: DoublyLinkedList[A], next: DoublyLinkedList[A]) extends DoublyLinkedList[A]



case class Stock(symbol: String, price: Double)
case class StockOption(symbol: String, strikePrice: Double, expiryDate: String)
case class Trade(id: String, amount: Double)

case class DLList[A](head: DoublyLinkedList[A], tail: DoublyLinkedList[A])

object DLList {
  def fromList[A](elements: List[A]): DLList[A] = {
    val dl = elements.foldLeft(Empty: DoublyLinkedList[A])(_.add(_))
    DLList(dl, findTail(dl))
  }

  private def findTail[A](list: DoublyLinkedList[A]): DoublyLinkedList[A] = list match {
    case Node(_, _, Empty) => list
    case Node(_, _, next)  => findTail(next)
    case Empty             => Empty
  }
}

@main def traverseList(): Unit = {
  val list = Empty
    .add("A")
    .add("B")
    .add("C")

  println("Traversing forward:")
  traverseForward(list)

  println("\nTraversing backward:")
  traverseBackward(findTail(list))
}

def traverseForward[A](list: DoublyLinkedList[A]): Unit = list match {
  case Empty => println("End of list")
  case Node(value, _, next) =>
    println(value)
    traverseForward(next)
}

def traverseBackward[A](list: DoublyLinkedList[A]): Unit = list match {
  case Empty => println("Start of list")
  case Node(value, prev, _) =>
    println(value)
    traverseBackward(prev)
}

def findTail[A](list: DoublyLinkedList[A]): DoublyLinkedList[A] = list match {
  case Node(_, _, Empty) => list // This is the tail
  case Node(_, _, next)  => findTail(next)
  case Empty             => Empty // Edge case for empty list
}

@main def modifyDLList(): Unit = {
  val dlList = DLList.fromList(List(1, 2, 3, 4))

  // Add a new value
  val newList = dlList.head.add(5)
  println(s"List after adding 5: ${newList.prettyPrint}")

  // Modify a value
  val modifiedList = newList.modifyWithPredicate((x: Int) => x == 3, (x: Int) => x * 10) // Multiply "3" by 10
  println(s"List after modifying 3 to 30: ${modifiedList.prettyPrint}")

  // Delete a value
  val deletedList = modifiedList.deleteWithPredicate((x: Int) => x == 2) // Remove "2"
  println(s"List after deleting 2: ${deletedList.prettyPrint}")
}

@main def useDLList(): Unit = {
  // Create a DLList from a Scala List
  val dlList = DLList.fromList(List(1, 2, 3, 4))

  // Access head and tail
  println("Head of the list:")
  println(dlList.head.prettyPrint)

  println("Tail of the list:")
  println(dlList.tail.prettyPrint)

  // Traverse forward
  println("\nTraversing forward:")
  traverseForward(dlList.head)

  // Traverse backward
  println("\nTraversing backward:")
  traverseBackward(dlList.tail)
}

@main def testFlatMap(): Unit = {
  val list = Empty
    .add(1)
    .add(2)
    .add(3)

  println(s"Original List: ${list.prettyPrint}")

  val doubledForward = list.flatMapForward(x => Empty.add(x * 2))
  println(s"Doubled (Forward): ${doubledForward.prettyPrint}")

  val doubledBackward = list.flatMapBackward(x => Empty.add(x * 2))
  println(s"Doubled (Backward): ${doubledBackward.prettyPrint}")
}
@main def testDoublyLinkedList(): Unit = {
    // Create a list of stocks
    val stockList: DoublyLinkedList[Stock] = Empty
        .add(Stock("AAPL", 150.0))
        .add(Stock("GOOGL", 2800.0))
        .add(Stock("AMZN", 3400.0))

    // Create a list of options
    val optionList: DoublyLinkedList[StockOption] = Empty
        .add(StockOption("AAPL", 155.0, "2024-01-01"))
        .add(StockOption("GOOGL", 2850.0, "2024-06-01"))

    println(s"Option List: ${optionList.prettyPrint}")
    // Modify a stock
    val modifiedStockList = stockList.modify(Stock("AAPL", 150.0), Stock("AAPL", 160.0))
    println(s"Modified Stock List: $modifiedStockList")

    // Delete a stock
    val deletedStockList = modifiedStockList.delete(Stock("GOOGL", 2800.0))
    println(s"Deleted Stock List: $deletedStockList")

    // FlatMap example
    def expandStock(stock: Stock): DoublyLinkedList[Stock] = Empty
        .add(Stock(stock.symbol + "_1", stock.price * 1.1))
        .add(Stock(stock.symbol + "_2", stock.price * 1.2))

    val expandedStockListForward = stockList.flatMapForward(expandStock)
    println(s"Expanded Stock List Forward: ${expandedStockListForward.prettyPrint}")
    
    val expandedStockListBackward = stockList.flatMapBackward(expandStock)
    println(s"Expanded Stock List Backward: ${expandedStockListBackward.prettyPrint}")

    // Create a trade list
    val tradeList: DoublyLinkedList[Trade] = Empty
        .add(Trade("T1", 10000.0))
        .add(Trade("T2", 15000.0))

    println(s"Trade List: ${tradeList.prettyPrint}")
    
}
