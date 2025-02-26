import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import java.util.UUID

object SaltedHash {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder().appName("SaltedHash").master("local[*]").getOrCreate()

    spark.sparkContext.setLogLevel("WARN")


    import spark.implicits._

    val data = Seq(
      ("Alice", 30, "NY"),
      ("Bob", 35, "CA"),
      ("Charlie", 40, "TX")
    ).toDF("name", "age", "state")

    val dfWithSaltedHash = data.withColumn("salt", lit(UUID.randomUUID().toString))
      .withColumn("salted_hash", sha2(concat(col("name"), lit("|"), col("age"), lit("|"), col("state"), lit("|"), col("salt")), 256))

    dfWithSaltedHash.show()
    
   val existingData = Seq(
      ("Alice", 30, "NY"),
      ("Bob", 35, "CA"),
      ("Charlie", 40, "TX")
    ).toDF("name", "age", "state").withColumn("salt", lit(UUID.randomUUID().toString)).withColumn("salted_hash", sha2(concat(col("name"), lit("|"), col("age"), lit("|"), col("state"), lit("|"), col("salt")), 256))
    existingData.show()
    // Simulate new data
    val newData = Seq(
      ("Alice", 30, "NY"),
      ("David", 45, "WA"),
      ("Charlie", 40, "TX")
    ).toDF("name", "age", "state")

     // 1. Join to get the salt.
    val joinedData = newData.join(existingData, Seq("name", "age", "state"), "left_outer")
    val newDataWithSaltedHash = joinedData.withColumn(
      "salted_hash",
      sha2(concat(col("name"), lit("|"), col("age"), lit("|"), col("state"), lit("|"), col("salt")), 256)
    )

    // 3. Perform a join to compare hashes
    val matchingRows = existingData.join(newDataWithSaltedHash, Seq("salted_hash"), "inner")
    
    // 4. Display the matching rows
    println("Matching Rows:")
    matchingRows.show()  

    // 5. Precompute Hashes on Driver Side
    val computedHashes = newDataWithSaltedHash.select("name", "age", "state", "salt", "salted_hash").collect()

    // 6. Verify the results of the join
    matchingRows.collect().foreach(row => {
      val name = row.getAs[String]("name")
      val age = row.getAs[Int]("age")
      val state = row.getAs[String]("state")
      val salt = row.getAs[String]("salt")
      val saltedHash = row.getAs[String]("salted_hash")

      val calculatedHash = computedHashes
        .find(r => r.getAs[String]("name") == name &&
          r.getAs[Int]("age") == age &&
          r.getAs[String]("state") == state &&
          r.getAs[String]("salt") == salt)
        .map(_.getAs[String]("salted_hash"))
        .getOrElse("")

      val isValid = saltedHash == calculatedHash
      println(s"Verification of $name: $isValid")
    })
 
    
    spark.stop()
  }
}