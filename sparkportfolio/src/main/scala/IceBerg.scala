import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.iceberg.spark.SparkSessionCatalog
import org.apache.spark.sql.{SparkSession, DataFrame}
import org.apache.iceberg.Table
import org.apache.iceberg.spark.Spark3Util

object IcebergExample {
    val tableName = "iceberg_catalog.iceberg_table"
/** 1. Create an Iceberg Table */
    def createIcebergTable(spark: SparkSession): Unit = {
        /*
        spark.sql(s"""
        CREATE TABLE IF NOT EXISTS $tableName (
            id INT,
            name STRING,
            age INT
        ) USING ICEBERG
        """)
        */
        import spark.implicits._
        val tableExists = spark.catalog.tableExists(tableName)
        if (!tableExists) {
            // Create the table only if it doesn't exist
            val data = Seq((1, "Alice", 25), (2, "Bob", 30), (3, "Charlie", 28)).toDF("id", "name", "age")
            data.writeTo(tableName).create()
        } else {
            println(s"Table '$tableName' already exists.")
        }
          
    }

    /** 2. Write Initial Data */
    def writeInitialData(spark: SparkSession): Unit = {
        import spark.implicits._
        val df = Seq((1, "Alice", 30), (2, "Bob", 35)).toDF("id", "name", "age")
        df.write.format("iceberg").mode("append").save(tableName)
        //df.writeTo(tableName).append()
    }

    /** 3. Append New Data to Create a New Version */
    def appendNewData(spark: SparkSession): Unit = {
        import spark.implicits._
        val df = Seq((3, "Charlie", 40)).toDF("id", "name", "age")
        df.write.format("iceberg").mode("append").save(tableName)
        //df.writeTo(tableName).append()
    }

    /** 4. Fetch Snapshot History */
    def getSnapshotHistory(spark: SparkSession): DataFrame = {
        spark.sql(s"SELECT parent_id, snapshot_id, made_current_at,is_current_ancestor FROM $tableName.history")
        
        
    }

    /** 5. Query Historical Data Using Time Travel */
    def queryHistoricalData(spark: SparkSession, snapshotId: Long): DataFrame = {
        spark.read
        .format("iceberg")
        .option("snapshot-id", snapshotId)
        .load(tableName)
    }

    /** 6. Rollback to a Previous Snapshot */
    def rollbackToSnapshot(spark: SparkSession, snapshotId: Long): Unit = {
        spark.sql(s"CALL iceberg_catalog.system.rollback_to_snapshot('$tableName', $snapshotId)")
    }

    def main(args: Array[String]): Unit = {
        val spark = SparkSession.builder()
        .appName("IcebergExample")
        .master("local[*]")
        .config("spark.sql.catalog.local", "org.apache.iceberg.spark.SparkCatalog") // Define custom Iceberg catalog
        .config("spark.sql.extensions", "org.apache.iceberg.spark.extensions.IcebergSparkSessionExtensions")
        .config("spark.sql.catalog.iceberg_catalog", "org.apache.iceberg.spark.SparkCatalog")
        .config("spark.sql.catalog.iceberg_catalog.type", "hadoop")
        .config("spark.sql.warehouse.dir", "file:///C:/tmp/spark-warehouse") // Use a valid path
        .config("spark.sql.catalog.iceberg_catalog.warehouse", "file:///C:/tmp/iceberg_warehouse")
          
        .getOrCreate()
        spark.sparkContext.setLogLevel("WARN")
        
        
        /*
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


        val parTableExists = spark.catalog.tableExists("iceberg_catalog.partitioned_table")
        if (!parTableExists) {
            // Create the table only if it doesn't exist
            val partitionedData = Seq(("2023-01-01", "productA", 10), ("2023-01-02", "productB", 15), ("2023-01-01", "productC", 20)).toDF("date", "product", "sales")
            partitionedData.writeTo("iceberg_catalog.partitioned_table").partitionedBy($"date").create()

        } else {
            println("Table 'iceberg_catalog.partitioned_table' already exists.")
            spark.read.table("iceberg_catalog.partitioned_table").show()
        }


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
        */

        // 1. Create Table
        createIcebergTable(spark)

        // 2. Write Initial Data
        writeInitialData(spark)

        // 3. Append New Data
        appendNewData(spark)
        spark.sql(s"DESCRIBE $tableName.history").show()
        // 4. Fetch Snapshot History
        val history = getSnapshotHistory(spark)
        history.show(false)

        // 5. Time Travel - Query Historical Data
        val firstSnapshotId = history.select("snapshot_id").first().getLong(0)
        println(s"Querying first snapshot: $firstSnapshotId")
        queryHistoricalData(spark, firstSnapshotId).show()

        // 6. Rollback to a Previous Version
        rollbackToSnapshot(spark, firstSnapshotId)

        println(s"Rolled back to snapshot: $firstSnapshotId")

        // 4. Read the table after rollback
        spark.read.table(tableName).show()
        
        spark.stop()
    }
}