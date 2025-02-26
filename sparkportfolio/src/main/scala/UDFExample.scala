import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
//import spark.implicits._

object UDFExample {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder().master("local").getOrCreate
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._

    // Define a Scala function
    def doubleValue(x: Int): Int = {
      x * 2
    }

    // Convert the Scala function to a UDF
    val doubleUDF = udf(doubleValue _)

    // Create a DataFrame
    val data = Seq((1), (2), (3)).toDF("value")

    // Use the UDF in a DataFrame transformation
    val resultDF = data.withColumn("doubled", doubleUDF(col("value")))

    resultDF.show()

    //Register the UDF so that it can be used in SQL.
    spark.udf.register("double_udf",doubleValue _)

    //Use the UDF in a SQL query.
    data.createOrReplaceTempView("my_table")
    spark.sql("""
              SELECT translate('AaBbCc', 'abc', '123') AS Translated1, 
              translate('AabcBbCc', 'abc', '123') AS Translated2,
              trunc('2019-08-04', 'Year') AS  BeginYear,
              trunc('2019-08-04', 'quarter') AS BeginvQuarter,
              trunc('2019-08-04', 'MONTH') AS BeginMonth,
              trunc('2019-08-04', 'week') AS BeginWeek,
              value, double_udf(value) 
              FROM my_table """).show()

    spark.stop()
  }
}