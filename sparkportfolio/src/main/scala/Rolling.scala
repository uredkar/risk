import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.expressions.Window
import org.apache.spark.sql.functions._
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object RollingStats {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder()
      .appName("Rolling Weekly Stats")
      .master("local[*]")
      .getOrCreate()
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._

    // Generate Sample Data (Stock Returns)
    val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val data = Seq(
      ("2024-02-01", 0.01),
      ("2024-02-02", -0.02),
      ("2024-02-03", 0.015),
      ("2024-02-04", 0.005),
      ("2024-02-05", 0.02),
      ("2024-02-06", -0.01),
      ("2024-02-07", 0.03),
      ("2024-02-08", -0.015),
      ("2024-02-09", 0.025),
      ("2024-02-10", -0.005),
      ("2024-02-11", 0.02),
      ("2024-02-12", -0.03),
      ("2024-02-13", 0.015),
      ("2024-02-14", 0.01)
    ).toDF("date", "return")

    // Convert date column to proper date format
    val df = data.withColumn("date", to_date(col("date"), "yyyy-MM-dd"))

    // Define Rolling Window (Last 7 Days)
    val rollingWindow = Window.orderBy(col("date")).rowsBetween(-6, 0)

    // Compute Rolling Weekly Average & Standard Deviation
    val dfWithStats = df.withColumn("rolling_avg", avg("return").over(rollingWindow))
      .withColumn("rolling_stddev", stddev("return").over(rollingWindow))

    // Show Result
    dfWithStats.show()
     // Compute Cumulative Returns (assuming initial investment = 1)
    val dfWithCumulative = df.withColumn("cumulative_return", 
      expr("exp(sum(ln(1 +  `return`)) over (order by date))")
    )

    // Compute Running Maximum of Cumulative Returns
    val windowSpec = Window.orderBy("date").rowsBetween(Window.unboundedPreceding, Window.currentRow)
    val dfWithRunningMax = dfWithCumulative.withColumn("running_max", max("cumulative_return").over(windowSpec))

    // Compute Drawdown (Percentage Drop from Peak)
    val dfWithDrawdown = dfWithRunningMax.withColumn("drawdown", 
      (col("cumulative_return") - col("running_max")) / col("running_max")
    )

    // Compute Maximum Drawdown
    val maxDrawdown = dfWithDrawdown.agg(min("drawdown").alias("max_drawdown")).collect()(0)(0)

    // Show the Results
    dfWithDrawdown.show()
    println(s"📉 Maximum Drawdown: $maxDrawdown")
    // Save to CSV for plotting
    dfWithDrawdown.select("date", "cumulative_return", "drawdown")
      .write
      .mode("overwrite")
      .option("header", "true")
      .csv("drawdown_output")

    println("✅ Drawdown data saved to 'drawdown_output/' for plotting.")
    spark.stop()
  }
}
