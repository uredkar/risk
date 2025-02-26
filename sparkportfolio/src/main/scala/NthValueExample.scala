import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.spark.sql.expressions.Window

object NthValueExample {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder().master("local").getOrCreate
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._
    {
      val data = Seq(
        ("Electronics", "Laptop", 1200.0),
        ("Electronics", "Phone", 800.0),
        ("Electronics", "Headphones", 150.0),
        ("Clothing", "Shirt", 50.0),
        ("Clothing", "Pants", 80.0),
        ("Clothing", "Jacket", 120.0)
      ).toDF("category", "product", "sales")

      val windowSpec = Window.partitionBy("category").orderBy(desc("sales"))
                          .rowsBetween(Window.unboundedPreceding, Window.unboundedFollowing)

      val resultDF = data.withColumn("second_highest_sale", nth_value(col("sales"), 2).over(windowSpec))

      resultDF.show()
    }
    {

      val data = Seq(
        ("ProductA", 100.0),
        ("ProductB", 200.0),
        ("ProductC", 150.0),
        ("ProductD", 300.0),
        ("ProductE", 250.0),
        ("ProductF", 180.0)
      ).toDF("product", "sales")

      val windowSpec = Window.orderBy("sales")

      val resultDF = data.withColumn("quartile", ntile(4).over(windowSpec))

      resultDF.show()
    }
    {
      val data = Seq(
        ("ClassA", "Alice", 85.0),
        ("ClassA", "Bob", 92.0),
        ("ClassA", "Charlie", 78.0),
        ("ClassB", "David", 90.0),
        ("ClassB", "Eve", 88.0),
        ("ClassB", "Frank", 95.0)
      ).toDF("class", "student", "score")

      val windowSpec = Window.partitionBy("class").orderBy("score")

      val resultDF = data.withColumn("percentile_rank", percent_rank().over(windowSpec))

      resultDF.show()
    }
    
    spark.stop()
  }
}