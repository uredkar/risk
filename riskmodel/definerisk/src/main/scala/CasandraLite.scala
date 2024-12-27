package casandralite.com

import scala.collection.concurrent.TrieMap
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Random, Try}
import scala.concurrent.duration._
import scala.concurrent.Await
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.io.Source

case class Symbol(name: String, initialPrice: Double, var price: Double, var circuitBroken: Boolean = false)
case class Trader(id: String, var cash: Double, positions: TrieMap[String, Int]) // symbol -> quantity
case class Order(traderId: String, orderId: String, symbol: String, quantity: Int, isBuy: Boolean)
//case class TradeOrder(traderId: String, orderId: String, symbol: String, quantity: Int, isBuy: Boolean)

object FileUtil {
  def appendToFile(filePath: String, content: String): Unit = {
    Files.write(
      Paths.get(filePath),
      (content + System.lineSeparator()).getBytes,
      StandardOpenOption.CREATE,
      StandardOpenOption.APPEND
    )
  }

  def readFile(filePath: String): List[String] = {
    if (Files.exists(Paths.get(filePath))) {
      Source.fromFile(filePath).getLines().toList
    } else {
      List.empty
    }
  }

  def writeToFile(filePath: String, content: String): Unit = {
    Files.write(
      Paths.get(filePath),
      content.getBytes,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
  }
}

// --- Schema Loader ---
object SchemaLoader {
  def loadSymbols(): List[Symbol] = {
    List(
      Symbol("AAPL", 10.0, 10.0),
      Symbol("GOOG", 15.0, 15.0),
      Symbol("MSFT", 12.0, 12.0)
    )
  }

  def loadTraders(): List[Trader] = {
    (1 to 4).toList.map { id =>
      Trader(s"Trader$id", Random.between(1000.0, 10000.0), TrieMap.empty)
    }
  }
}

// --- Order Book ---
object OrderBook {
  private val orders = TrieMap[String, List[Order]]().withDefaultValue(Nil)

  def placeOrder(order: Order): Unit = synchronized {
    orders(order.symbol) = orders(order.symbol) :+ order
  }

  def getOrders(symbol: String): List[Order] = orders(symbol)

  def clearOrders(symbol: String): Unit = synchronized {
    orders(symbol) = Nil
  }
}

// --- Node ---
class Node(id: String, traders: List[Trader], symbols: List[Symbol],val logFilePath: String)(using ExecutionContext) {
  private val logs = TrieMap[Int, String]()
  private var logIndex = 0
  
  def getTraders: List[Trader] = traders
  
  def addLog(message: String): Unit = {
    val timestampedMessage = s"${java.time.Instant.now()} - $message"
    FileUtil.appendToFile(logFilePath, timestampedMessage)
  }

  def submitOrder(order: Order): Unit = {
    val traderOpt = traders.find(_.id == order.traderId)
    val symbolOpt = symbols.find(_.name == order.symbol)

    (traderOpt, symbolOpt) match {
      case (Some(trader), Some(symbol)) if !symbol.circuitBroken =>
        val cost = symbol.price * order.quantity
        if (order.isBuy && trader.cash >= cost) {
          trader.cash -= cost
          trader.positions.updateWith(order.symbol) {
            case Some(qty) if qty + order.quantity >= 0 => Some(qty + order.quantity)
            case Some(qty) =>
              log(s"Invalid sell order: ${order.traderId} tried to sell more than they own.")
              Some(qty) // Keep the current quantity
            case None => 
              log(s"Invalid order: ${order.traderId} has no position in ${order.symbol}.")
              None
          }
          log(s"${order.traderId} bought ${order.quantity} of ${order.symbol} at ${symbol.price}")
        } else if (!order.isBuy && trader.positions.getOrElse(order.symbol, 0) + order.quantity >= 0) {
          trader.cash += cost
          trader.positions.updateWith(order.symbol) {
            case Some(qty) if qty + order.quantity >= 0 => Some(qty + order.quantity)
            case None => Some(order.quantity)
          }
          log(s"${order.traderId} sold ${order.quantity} of ${order.symbol} at ${symbol.price}")
        } else {
          log(s"${order.traderId} failed to place order for ${order.symbol}")
          return
        }

        OrderBook.placeOrder(order)

      case (Some(_), Some(symbol)) if symbol.circuitBroken =>
        log(s"Order rejected: ${symbol.name} trading halted.")
      case _ =>
        log(s"Order rejected: Invalid trader or symbol.")
    }
    saveTradersState()
  }

  def processOrders(): Unit = {
    symbols.foreach { symbol =>
      if (!symbol.circuitBroken) {
        val orders = OrderBook.getOrders(symbol.name)
        val (buys, sells) = orders.partition(_.isBuy)
        
        val netQuantity = buys.map(_.quantity).sum - sells.map(_.quantity).sum

        if (netQuantity > 0) symbol.price *= 1.01
        else if (netQuantity < 0) symbol.price *= 0.99

        symbol.price = Math.max(0.01, symbol.price)

        if (symbol.price > symbol.initialPrice * 1.10 || symbol.price < symbol.initialPrice * 0.90) {
          symbol.circuitBroken = true
          log(s"Trading halted for ${symbol.name}. Current price: ${symbol.price}")
        }

        OrderBook.clearOrders(symbol.name)
      }
    }
  }

  def log(message: String): Unit = synchronized {
    logs.update(logIndex, message)
    logIndex += 1
    val timestampedMessage = s"${java.time.Instant.now()} - $message"
    FileUtil.appendToFile(logFilePath, timestampedMessage)
    //println(message)
  }

  def getLogs: List[String] = logs.values.toList

  def saveTradersState(): Unit = {
    traders.foreach { trader =>
      val filePath = s"${trader.id}_positions.txt"
      val content = trader.positions.map { case (symbol, qty) => s"$symbol: $qty" }.mkString("\n")
      //println(s"saveTradersState id ${trader.id} cash ${trader.cash} position ${trader.positions}")
      val timestampedMessage = s"${java.time.Instant.now()}: $content"
      FileUtil.writeToFile(filePath, timestampedMessage)
    }
  }
  

  def synchronizeData(otherNode: Node): Unit = {
    // Sync logs
    val otherLogs = FileUtil.readFile(otherNode.logFilePath)
    otherLogs.foreach(log)

    // Sync traders' states
    otherNode.getTraders.foreach { trader =>
      val filePath = s"${trader.id}_positions.txt"
      val otherTraderData = FileUtil.readFile(filePath)
      if (otherTraderData.nonEmpty) {
        val mergedPositions = otherTraderData.map { line =>
          val Array(timestamp,symbol, qty) = line.split(": ").map(_.trim)
          symbol -> qty.toInt
        }.toMap

        trader.positions ++= mergedPositions
      }
    }
  }
}

// --- Trading Application ---
object TradingApp {
  def run(node: Node, traders: List[Trader], symbols: List[Symbol]): Unit = {
    val randomTrader = traders(Random.nextInt(traders.size))
    val randomSymbol = symbols(Random.nextInt(symbols.size))

    val isBuy = Random.nextBoolean()
    val quantity = Random.between(1, 10)

    val order = Order(randomTrader.id, java.util.UUID.randomUUID().toString, randomSymbol.name, quantity, isBuy)
    node.submitOrder(order)
  }
}

def printTraderState(label: String, traders: List[Trader]): Unit = {
  println(s"\n$label")
  traders.foreach { trader =>
    println(s"Trader: ${trader.id}, Cash: ${trader.cash}, Positions: ${trader.positions.toMap}")
  }
}


@main def testCassandraLite200(): Unit = {
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  val symbols = SchemaLoader.loadSymbols()
  val traders = SchemaLoader.loadTraders()
  //val node = Node("TradingNode", traders, symbols)
  val node1 = Node("Node1", traders, symbols, "node1_logs.txt")
  val node2 = Node("Node2", traders, symbols, "node2_logs.txt")
  val startTime = System.nanoTime()

  val simulationFuture = Future {
    while (TimeUnit.NANOSECONDS.toSeconds(System.nanoTime() - startTime) < 60) { // Run for up to 60 seconds
      TradingApp.run(node1, traders, symbols)
      TradingApp.run(node2, traders, symbols)
      node1.processOrders()
      node2.processOrders()
      Thread.sleep(100)
    }
  }

  Await.result(simulationFuture, 120.seconds)
}

// --- Testing ---
@main def testCassandraLite100(): Unit = {
  given ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  val symbols = SchemaLoader.loadSymbols()
  val traders = SchemaLoader.loadTraders()

  //val node = Node("TradingNode", traders, symbols)
  val node1 = Node("Node1", traders, symbols, "node1_logs.txt")
  val node2 = Node("Node2", traders, symbols, "node2_logs.txt")
  // Print initial state
  printTraderState("Initial State", traders)

  
   // Futures for Node1
  val node1TradingFuture = Future {
    (1 to 100).foreach { _ =>
      TradingApp.run(node1, traders, symbols)
      Thread.sleep(100)
    }
  }

  val node1ProcessingFuture = Future {
    (1 to 100).foreach { _ =>
      node1.processOrders()
      Thread.sleep(500)
    }
  }

  // Futures for Node2
  val node2TradingFuture = Future {
    (1 to 100).foreach { _ =>
      TradingApp.run(node2, traders, symbols)
      Thread.sleep(100)
    }
  }

  val node2ProcessingFuture = Future {
    (1 to 100).foreach { _ =>
      node2.processOrders()
      Thread.sleep(500)
    }
  }

   // Await for all futures to complete
  Await.result(
    Future.sequence(Seq(node1TradingFuture, node1ProcessingFuture, node2TradingFuture, node2ProcessingFuture)),
    120.seconds
  )

  // Synchronize data between nodes
  println("Synchronizing data between Node1 and Node2...")
  node1.synchronizeData(node2)
  node2.synchronizeData(node1)


  //Await.result(Future { Thread.sleep(20000) }, 30.seconds)
  //Await.result(Future.sequence(Seq(tradingFuture, processingFuture)), 60.seconds)
  // Print final state
  //node1.synchronizeData(node2)
  printTraderState("Final State", traders)
}
