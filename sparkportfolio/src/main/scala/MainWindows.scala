


import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.spark.sql.expressions.Window
import org.apache.spark.sql.avro._

import scala3encoders.given

  // Define transformations
  sealed trait Transformation
  case class AddColumn(name: String, expression: org.apache.spark.sql.Column) extends Transformation
  case class DropColumn(name: String) extends Transformation
  case class RenameColumn(oldName: String, newName: String) extends Transformation


object WindowFunctions extends App {

  val spark: SparkSession = SparkSession.builder()
    .master("local[1]")
    .appName("SparkByExamples.com")
    .config("spark.sql.warehouse.dir", "file:///C:/tmp/spark-warehouse") // Use a valid path
    .config("spark.local.dir", "C:/tmp")
    .getOrCreate()

  spark.sparkContext.setLogLevel("ERROR")

  import spark.implicits._

  val simpleData = Seq(("James", "Sales", 3000),
    ("Michael", "Sales", 4600),
    ("Robert", "Sales", 4100),
    ("Maria", "Finance", 3000),
    ("James", "Sales", 3000),
    ("Scott", "Finance", 3300),
    ("Jen", "Finance", 3900),
    ("Jeff", "Marketing", 3000),
    ("Kumar", "Marketing", 2000),
    ("Saif", "Sales", 4100)
  )
  val df = simpleData.toDF("employee_name", "department", "salary")
  df.show()

  //row_number
  val windowSpec  = Window.partitionBy("department").orderBy("salary")
  df.withColumn("row_number",row_number.over(windowSpec))
    .show()

  //rank
  df.withColumn("rank",rank().over(windowSpec))
    .show()


  //dens_rank
  df.withColumn("dense_rank",dense_rank().over(windowSpec))
    .show()

  //percent_rank
  df.withColumn("percent_rank",percent_rank().over(windowSpec))
    .show()

  //ntile
  df.withColumn("ntile",ntile(2).over(windowSpec))
    .show()

  //cume_dist
  df.withColumn("cume_dist",cume_dist().over(windowSpec))
    .show()

  //lag
  df.withColumn("lag",lag("salary",2).over(windowSpec))
    .show()

  //lead
  df.withColumn("lead",lead("salary",2).over(windowSpec))
    .show()

  //Aggregate Functions
  val windowSpecAgg  = Window.partitionBy("department")
  val aggDF = df.withColumn("row",row_number.over(windowSpec))
    .withColumn("avg", avg(col("salary")).over(windowSpecAgg))
    .withColumn("sum", sum(col("salary")).over(windowSpecAgg))
    .withColumn("min", min(col("salary")).over(windowSpecAgg))
    .withColumn("max", max(col("salary")).over(windowSpecAgg))
    .where(col("row")===1).select("department","avg","sum","min","max")
    .show()
    //aggDF.write.format("csv").mode("overwrite").save("c:/tmp/window")    
  //c:/tmp/wordCount
    //aggDF.write.avro("C:/sources/risk/sparkportfolio/spark-warehouse/window")    

  println("================== reading from file")
  val dfRead = spark.read
          .format("csv")
          .option("header", "true") //first line in file has headers
          .option("inferSchema", "true") 
          .load("c:/tmp/window/part-00000-eb5df69f-f66f-40f1-a4af-17278785a743-c000.csv")
  dfRead.show()
}

@main def readcustomer() = 
  val spark: SparkSession = SparkSession.builder()
    .master("local[1]")
    .appName("SparkByExamples.com")
    .config("spark.sql.warehouse.dir", "file:///C:/tmp/spark-warehouse") // Use a valid path
    .config("spark.local.dir", "C:/tmp")
    .getOrCreate()

  spark.sparkContext.setLogLevel("ERROR")

  import spark.implicits._

  println("================== reading from file")
  val dfRead = spark.read
          .format("csv")
          .option("header", "true") //first line in file has headers
          .option("inferSchema", "true") 
          .option("quote","'")
          .load("c:/tmp/customer.csv")
  dfRead.show()
  //val df = dfRead.withColumn("ORDER_AMT", rtrim(col("ORDER_AMT")))

  val transformations = Seq(
      
      AddColumn("DISCOUNT", col("ORDER_AMT") * 0.90),
      //AddColumn("cat", when(col("salary") > 2000,"high").otherwise("normal")),
      AddColumn("TIER_DISCOUNT", 
                      when(col("CUST_TIER") === lit("Gold"),col("ORDER_AMT") * 0.80).otherwise(col("ORDER_AMT") * 0.90)),
      RenameColumn("CUST_ID", "ID"),
      DropColumn("ID"),
      DropColumn("CUST_NAME"),
      
    )

  // Apply transformations
  val resultDf = ETLTransformer.transform(dfRead, transformations, Some(Seq("CUST_TIER")))

  // Show the result
  resultDf.show()
  spark.close()

import org.apache.spark.sql.{DataFrame, Dataset, SparkSession}
import org.apache.spark.sql.functions._

object ETLTransformer {

  /**
   * Generic transform method for DataFrame/Dataset
   *
   * @param inputDf     The input DataFrame or Dataset
   * @param transformations A sequence of transformations to apply
   *                        - Add: Adds a new column using a Spark SQL expression
   *                        - Drop: Drops a column
   *                        - GroupBy: Groups by specific columns and applies aggregates
   * @param groupByCols Optional columns to group by (for aggregations)
   * @return Transformed DataFrame
   */
  def transform(
      inputDf: DataFrame,
      transformations: Seq[Transformation],
      groupByCols: Option[Seq[String]] = None
  ): DataFrame = {
    // Apply column transformations
    val transformedDf = transformations.foldLeft(inputDf) { (df, transformation) =>
      transformation match {
        case AddColumn(name, expression) =>
          df.withColumn(name, expression)

        case DropColumn(name) =>
          df.drop(name)

        case RenameColumn(oldName, newName) =>
          df.withColumnRenamed(oldName, newName)

        
      }
    }

    // Apply group by and aggregation if specified
    groupByCols match {
      case Some(cols) =>
        // Placeholder for aggregation logic, aggregating all numeric columns with sum
        val aggExprs = transformedDf.columns
          .filterNot(cols.contains)
          .map(colName => sum(col(colName)).as(s"sum_$colName"))

        transformedDf.groupBy(cols.map(col): _*).agg(aggExprs.head, aggExprs.tail: _*)

      case None => transformedDf
    }
  }


  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder()
      .appName("ETLTransformer")
      .master("local[*]")
      .getOrCreate()

    import spark.implicits._

    // Sample input data
    val data = Seq(
      ("Alice", 34, 2000.0),
      ("Bob", 45, 3000.0),
      ("Charlie", 23, 1500.0)
    )
    val inputDf = data.toDF("name", "age", "salary")
    inputDf.show()
    // Define transformations
    val transformations = Seq(
      AddColumn("bonus", col("salary") * 0.1),
      AddColumn("cat", when(col("salary") > 2000,"high").otherwise("normal")),
      RenameColumn("name", "employee_name"),
      DropColumn("age")
    )

    // Apply transformations
    val resultDf = transform(inputDf, transformations, Some(Seq("employee_name","cat")))

    // Show the result
    resultDf.show()
  }
}
