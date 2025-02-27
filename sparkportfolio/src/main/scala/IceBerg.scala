import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.iceberg.spark.SparkSessionCatalog
import org.apache.iceberg.Table

object IcebergExample {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder()
      .appName("IcebergExample")
      .master("local[*]")
      .config("spark.sql.extensions", "org.apache.iceberg.spark.extensions.IcebergSparkSessionExtensions")
      .config("spark.sql.catalog.iceberg_catalog", "org.apache.iceberg.spark.SparkCatalog")
      .config("spark.sql.catalog.iceberg_catalog.type", "hadoop")
      .config("spark.sql.catalog.iceberg_catalog.warehouse", "file:///tmp/iceberg_warehouse") // Adjust path
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._

    val tableExists = spark.catalog.tableExists("iceberg_catalog.my_table")
    if (!tableExists) {
        // Create the table only if it doesn't exist
        val data = Seq((1, "Alice", 25), (2, "Bob", 30), (3, "Charlie", 28)).toDF("id", "name", "age")
        data.writeTo("iceberg_catalog.my_table").create()
    } else {
        println("Table 'iceberg_catalog.my_table' already exists.")
    }
    // 1. Create a table
    
    

    // 2. Read the table
    val readDf = spark.read.table("iceberg_catalog.my_table")
    readDf.show()

    // 3. Insert new data
    val newData = Seq((4, "David", 35), (5, "Eve", 29)).toDF("id", "name", "age")
    newData.writeTo("iceberg_catalog.my_table").append()

    // 4. Read the updated table
    spark.read.table("iceberg_catalog.my_table").show()

    // 5. Time travel (read a previous version)
    val version = spark.sql("SELECT snapshot_id  FROM iceberg_catalog.my_table.history ORDER BY snapshot_id DESC LIMIT 1").first().getLong(0) -1;
    spark.read.option("version-id", version).table("iceberg_catalog.my_table").show()

    // 6. Partitioning (example)
    val partitionedData = Seq(("2023-01-01", "productA", 10), ("2023-01-02", "productB", 15), ("2023-01-01", "productC", 20)).toDF("date", "product", "sales")
    partitionedData.writeTo("iceberg_catalog.partitioned_table").partitionedBy($"date").create()

    spark.read.table("iceberg_catalog.partitioned_table").show()

    // 7. Overwrite Partition (example)
    val overwriteData = Seq(("2023-01-01", "productD", 30), ("2023-01-01", "productE", 35)).toDF("date", "product", "sales")
    overwriteData.writeTo("iceberg_catalog.partitioned_table").option("replace-where", "date = '2023-01-01'").createOrReplace()

    spark.read.table("iceberg_catalog.partitioned_table").show()

    // 8. Delete rows (example)
    spark.sql("DELETE FROM iceberg_catalog.my_table WHERE id = 1")
    spark.read.table("iceberg_catalog.my_table").show()

    // 9. Update rows (example)
    spark.sql("UPDATE iceberg_catalog.my_table SET age = 31 WHERE id = 2")
    spark.read.table("iceberg_catalog.my_table").show()

    spark.stop()
  }
}