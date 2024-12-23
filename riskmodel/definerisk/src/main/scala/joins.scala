case class Order(orderId: Int, customerId: Int)
case class OrderDetail(orderId: Int, productId: Int, price: Double, quantity: Int)
case class Customer(customerId: Int, name: String)

case class Row(columns: Map[String, Any]) {
  def get[T](column: String): Option[T] = columns.get(column).asInstanceOf[Option[T]]
}

case class Table(name: String, rows: List[Row])

extension (table: Table)
    def select(columns: String*): Table =
        Table(table.name, table.rows.map(row => Row(row.columns.filterKeys(columns.contains).toMap)))

    def filter(predicate: Row => Boolean): Table =
        Table(table.name, table.rows.filter(predicate))

    def join(other: Table, on: (String, String)): Table = {
        val (leftKey, rightKey) = on
        val joinedRows = for {
        leftRow <- table.rows
        rightRow <- other.rows
        if leftRow.get[Any](leftKey) == rightRow.get[Any](rightKey)
        } yield Row(leftRow.columns ++ rightRow.columns)
        Table(s"${table.name}_joined_${other.name}", joinedRows)
    }
    def aggregate(groupByKey: String, agg: (List[Row]) => Row): Table = {
        val grouped = table.rows.groupBy(_.get[Any](groupByKey))
        Table(s"${table.name}_aggregated", grouped.values.map(agg).toList)
    }

    def withColumn(newColumn: String, function: SqlFunction): Table =
        Table(
            table.name,
            table.rows.map { row =>
                Row(row.columns + (newColumn -> function(row)))
            }
        )

trait SqlFunction {
  def apply(row: Row): Any
}

case class Column(name: String) extends SqlFunction {
  def apply(row: Row): Any = row.get[Any](name).getOrElse(null)
}

case class Literal(value: Any) extends SqlFunction {
  def apply(row: Row): Any = value
}

case class Concat(columns: List[SqlFunction]) extends SqlFunction {
  def apply(row: Row): String =
    columns.map(_.apply(row).toString).mkString
}

case class When(condition: Row => Boolean, trueValue: SqlFunction, falseValue: SqlFunction) extends SqlFunction {
  def apply(row: Row): Any =
    if (condition(row)) trueValue.apply(row) else falseValue.apply(row)
}

val orders = List(
  Order(1, 101),
  Order(2, 102),
  Order(3, 101)
)

val orderDetails = List(
  OrderDetail(1, 201, 100.0, 2),
  OrderDetail(2, 202, 50.0, 3),
  OrderDetail(3, 203, 75.0, 1),
  OrderDetail(1, 204, 150.0, 1)
)

val customers = List(
  Customer(101, "myCustomer"),
  Customer(102, "anotherCustomer")
)

// Create maps for faster lookup
val ordersByCustomerId: Map[Int, List[Order]] = orders.groupBy(_.customerId)
val orderDetailsByOrderId: Map[Int, List[OrderDetail]] = orderDetails.groupBy(_.orderId)

// Define some example data
val customersTbl = Table("customers", List(
  Row(Map("id" -> 1, "name" -> "Alice")),
  Row(Map("id" -> 2, "name" -> "Bob"))
))

val ordersTbl = Table("orders", List(
  Row(Map("id" -> 1, "customerId" -> 1, "amount" -> 100.0)),
  Row(Map("id" -> 2, "customerId" -> 2, "amount" -> 200.0)),
  Row(Map("id" -> 3, "customerId" -> 1, "amount" -> 150.0))
))

// Query: Select customers with their total order amounts
val joinedTable = customersTbl
  .join(ordersTbl, ("id", "customerId"))
  .select("name", "amount")
  .filter(row => row.get[Double]("amount").exists(_ > 100))

val updatedCustomers = customersTbl.withColumn(
  "greeting",
  Concat(List(Literal("Hello, "), Column("name"), Literal("!")))
)



@main def join1() = 
    val total = customers
    .filter(_.name == "myCustomer")                        // Filter customers by name
    .flatMap { customer =>                                 
        orders.filter(_.customerId == customer.customerId)   // Get orders for the customer
    }
    .flatMap { order => 
        orderDetails.filter(_.orderId == order.orderId)      // Get order details for each order
    }
    .foldLeft(0.0) { (total, detail) => 
        total + detail.price * detail.quantity               // Calculate total price
    }

    println(s"Total price for myCustomer: $$${total}")

    val total2 = customers
        .filter(_.name == "myCustomer") // Find the specific customer
        .flatMap { customer =>
            ordersByCustomerId.getOrElse(customer.customerId, List()) // Lookup orders by customerId
    }
    .flatMap { order =>
        orderDetailsByOrderId.getOrElse(order.orderId, List()) // Lookup order details by orderId
    }
    .foldLeft(0.0) { (total, detail) =>
        total + detail.price * detail.quantity // Calculate total price
    }

    println(s"Total price for myCustomer: $$${total2}")

    // Print results
    joinedTable.rows.foreach { row =>
        println(s"Name: ${row.get[String]("name").getOrElse("")}, Amount: ${row.get[Double]("amount").getOrElse(0.0)}")
    }

    updatedCustomers.rows.foreach { row =>
        println(row.columns)
    }

    val ordersWithDiscount = ordersTbl.withColumn(
        "discountedAmount",
        When(
            row => row.get[Double]("amount").exists(_ > 150),
            Literal("Discount Applied"),
            Literal("No Discount")
        )
    )

    ordersWithDiscount.rows.foreach { row =>
        println(row.columns)
    }
