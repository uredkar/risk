
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.Row

object NestedRepeatedScala {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder()
      .appName("NestedRepeatedScala")
      .master("local[*]") // Use local mode for testing
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._

    // Define the schema with nested and repeated fields
    val schema = StructType(Seq(
      StructField("user_id", IntegerType, true),
      StructField("user_info", StructType(Seq(
        StructField("name", StringType, true),
        StructField("address", StructType(Seq(
          StructField("city", StringType, true),
          StructField("zip", StringType, true)
        )), true)
      )), true),
      StructField("orders", ArrayType(StructType(Seq(
        StructField("order_id", StringType, true),
        StructField("items", ArrayType(StringType, true), true),
        StructField("price", IntegerType, true)
      )), true), true)
    ))

    // Create sample data
    val data = Seq(
      (1, ("Alice", ("New York", "10001")), Seq(("ORD123", Seq("Book", "Pen"), 25), ("ORD456", Seq("Laptop"), 1200))),
      (2, ("Bob", ("Los Angeles", "90001")), Seq(("ORD789", Seq("Phone", "Charger"), 150))),
      (3, ("Charlie", ("Chicago", "60601")), Seq(("ORD101", Seq("Headphones"), 50), ("ORD102", Seq("Mouse", "Keyboard"), 100)))
    )

    

    val rowData = data.map { case (userId, userInfo, orders) =>
      Row(userId, Row(userInfo._1, Row(userInfo._2._1, userInfo._2._2)), orders.map { case (orderId, items, price) =>
        Row(orderId, items, price)
      })
    }

    val df = spark.createDataFrame(spark.sparkContext.parallelize(rowData), schema)

    df.printSchema()
    df.show(false)
    
    // Spark SQL Examples
    df.createOrReplaceTempView("users")

    // Access nested fields
    spark.sql("SELECT user_id, user_info.name, user_info.address.city FROM users").show()

    // Access nested struct within an array
    spark.sql("SELECT user_id, orders[0].order_id FROM users").show()

    // Explode the orders array
    spark.sql("SELECT user_id, explode(orders) as order FROM users").show()

    // Access nested elements after exploding the array
    spark.sql("SELECT user_id, order.order_id, explode(order.items) as item FROM (SELECT user_id, explode(orders) as order FROM users)").show()

    // Access nested data and apply filtering
    spark.sql("SELECT user_id, order.order_id FROM (SELECT user_id, explode(orders) as order FROM users) WHERE order.price > 100").show()

     // Simulate BigQuery's UNNEST functionality
    val unnestedOrders = df.select($"user_id", explode($"orders").as("order"))
      .select($"user_id", $"order.order_id".as("order_id"), $"order.items".as("items"), $"order.price".as("price"))

    unnestedOrders.show(false)

    // Simulate further unnesting to flatten the items array
    val unnestedItems = unnestedOrders.select($"user_id", $"order_id", explode($"items").as("item"), $"price")

    unnestedItems.show(false)

    // Simulate ARRAY_AGG of nested orders
    val aggregatedOrders = df.groupBy($"user_id").agg(collect_list($"orders").as("aggregated_orders"))

    aggregatedOrders.show(false);

    // Simulate ARRAY_AGG of nested items within orders
    val aggregatedItems = spark.sql("""
      SELECT
            user_id,
            collect_list(
                struct(
                    order_id,
                    aggregated_items,
                    price
                )
            ) as aggregated_orders_with_items
        FROM (
            SELECT
                user_id,
                order.order_id as order_id,
                collect_list(item) as aggregated_items,
                order.price as price
            FROM users
            LATERAL VIEW explode(orders) as order
            LATERAL VIEW explode(order.items) as item
            GROUP BY user_id, order.order_id, order.price
        ) subquery
        GROUP BY user_id
    """)

    aggregatedItems.show(false);

    spark.stop()
  }
}