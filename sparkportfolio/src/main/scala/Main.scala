
import org.apache.spark.sql.SparkSession

@main def helloSpark() = {
    
    val spark = SparkSession.builder().appName("helloSpark").master("local[*]").getOrCreate()
    spark.sparkContext.setLogLevel("WARN")

    spark.read.text("build.sbt").show()

}