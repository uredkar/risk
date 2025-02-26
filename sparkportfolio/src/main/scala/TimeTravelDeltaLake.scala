import org.apache.spark.sql.SparkSession
import io.delta.tables._

object TimeTravelExample {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder()
      .appName("TimeTravelExample")
      .master("local[*]") // Use "yarn" or "cluster" for production
      .config("spark.sql.extensions", "io.delta.sql.DeltaSparkSessionExtension")
      .config("spark.sql.catalog.spark_catalog", "org.apache.spark.sql.delta.catalog.DeltaCatalog")
      .config("spark.hadoop.io.native.lib.available", "false")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")

    import spark.implicits._

    // Define Delta table path
    val deltaTablePath = "data\\financial_reports"
    /*
    // Sample financial transactions (Insert-Only)
    val transactions = Seq(
      ("2025-02-01", "AAPL", 150.5, 10),
      ("2025-02-02", "AAPL", 152.0, 20),
      ("2025-02-02", "GOOG", 2800.0, 5),
      ("2025-02-03", "AAPL", 155.0, 15)
    ).toDF("date", "symbol", "price", "quantity")
    
    // Write initial data to Delta Table
    transactions.write.format("delta").mode("overwrite").save(deltaTablePath)
    println("First Write Done")
    
    // Append new data (insert-only)
    val newTransactions = Seq(
      ("2025-02-04", "TSLA", 3700.0, 38),
      ("2025-02-05", "GOOG", 3820.0, 31),
      ("2025-02-06", "GOOG", 3830.0, 32)
    ).toDF("date", "symbol", "price", "quantity")

    newTransactions.write.format("delta").mode("append").save(deltaTablePath)
    println("Second Write Done")
    //spark.sql("ALTER TABLE delta.`data/financial_reports` SET TBLPROPERTIES ('delta.logRetentionDuration' = '30 days', 'delta.deletedFileRetentionDuration' = '30 days')")
    
    */
    // Time Travel: Query data as of a past timestamp
    val timeQuery = spark.read.format("delta")
      .option("timestampAsOf", "2025-02-26 10:14:43") // Replace with actual timestamp
      .load(deltaTablePath)

    println("Data as of 2025-02-26:")
    timeQuery.show()
    
    // Time Travel: Query data as of a past version
    {
    val versionQuery = spark.read.format("delta")
      .option("versionAsOf", 1) // Replace with actual version number
      .load(deltaTablePath)

    println("Data as of version 1:")
    versionQuery.show()
    }
    {
    val versionQuery = spark.read.format("delta")
      .option("versionAsOf", 2) // Replace with actual version number
      .load(deltaTablePath)

    println("Data as of version 2:")
    versionQuery.show()
    }
    {
    val versionQuery = spark.read.format("delta")
      .option("versionAsOf", 3) // Replace with actual version number
      .load(deltaTablePath)

    println("Data as of version 3:")
    versionQuery.show()
    }
    // Get Delta table history
    val deltaTable = DeltaTable.forPath(spark, deltaTablePath)
    deltaTable.history().show()

    spark.stop()
  }
}
