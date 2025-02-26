package sql

//import buildinfo.BuildInfo.inputDirectory
import org.apache.spark.sql.{Dataset, Row, SparkSession}
import org.apache.spark.sql.functions.col
import org.apache.spark.sql.functions.split

//import scala3encoders.given

object StarWarsData {
  val spark = SparkSession.builder().master("local").getOrCreate
  spark.sparkContext.setLogLevel("WARN")
  import spark.implicits._

  case class Friends(name: String, friends: String)
  val friends: Dataset[Friends] = Seq(
    ("Yoda", "Obi-Wan Kenobi"),
    ("Anakin Skywalker", "Sheev Palpatine"),
    ("Luke Skywalker", "Han Solo, Leia Skywalker"),
    ("Leia Skywalker", "Obi-Wan Kenobi"),
    ("Sheev Palpatine", "Anakin Skywalker"),
    ("Han Solo", "Leia Skywalker, Luke Skywalker, Obi-Wan Kenobi, Chewbacca"),
    ("Obi-Wan Kenobi", "Yoda, Qui-Gon Jinn"),
    ("R2-D2", "C-3PO"),
    ("C-3PO", "R2-D2"),
    ("Darth Maul", "Sheev Palpatine"),
    ("Chewbacca", "Han Solo"),
    ("Lando Calrissian", "Han Solo"),
    ("Jabba", "Boba Fett")
  ).map(x => Friends.apply(x._1,x._2)).toDS

  case class FriendsMissing(who: String, friends: Option[String])
  val dsMissing: Dataset[FriendsMissing] = Seq(
    ("Yoda", Some("Obi-Wan Kenobi")),
    ("Anakin Skywalker", Some("Sheev Palpatine")),
    ("Luke Skywalker", Option.empty[String]),
    ("Leia Skywalker", Some("Obi-Wan Kenobi")),
    ("Sheev Palpatine", Some("Anakin Skywalker")),
    ("Han Solo", Some("Leia Skywalker, Luke Skywalker, Obi-Wan Kenobi"))
  ).map(x => FriendsMissing.apply(x._1,x._2)).toDS

  case class Character(
      name: String,
      height: Double,
      weight: Option[Double],
      eyecolor: Option[String],
      haircolor: Option[String],
      jedi: Boolean,
      species: String
  )

  val df = spark.sqlContext.read
    .option("header", "true")
    .option("delimiter", ",")
    .option("inferSchema", "true")
    .csv("C:/sources/newrisk/risk/sparkportfolio/spark-warehouse/starwars.csv")
    
  df.createOrReplaceTempView("starwars")
  


  // going via java interface here of DataFrame map function here
  val characters = df
    .select(
      classOf[Character].getDeclaredFields
        .filterNot(_.isSynthetic)
        .map(_.getName)
        .map(col): _*
    )
    .map {
      case Row(
            name: String,
            height: Double,
            weight: String,
            eyecolor: String,
            haircolor: String,
            jedi: String,
            species: String
          ) =>
        def getOpt(s: String): Option[String] = s match {
          case "NA" => None
          case s    => Some(s)
        }
        def getDoubleOpt(s: String): Option[Double] = getOpt(s).map(_.toDouble)
        
        Character(
          name,
          height,
          getDoubleOpt(weight),
          getOpt(eyecolor),
          getOpt(haircolor),
          jedi == "jedi",
          species
        )
    }

  val sw_df = characters.join(friends, Seq("name"))
  friends.createOrReplaceTempView("friends")
  characters.createOrReplaceTempView("characters")
  sw_df.createOrReplaceTempView("withfriends")

  case class SW(
      name: String,
      height: Double,
      weight: Option[Double],
      eyecolor: Option[String],
      haircolor: Option[String],
      jedi: String,
      species: String,
      friends: String
  )

  val sw_ds = sw_df.as[SW]

}

object StarWar {
  def main(args: Array[String]): Unit = {
    import StarWarsData._
    try {
      
      //friends.show()
      //dsMissing.show()
      //characters.show()
      //sw_ds.show()
      //spark.sql("select  to_date('2009-07-30 04:17:52') as tdate,current_date() as cd, eyecolor,species,count(*) as cnt from starwars group by eyecolor,species").show()
      //spark.sql("select name, split(friends,',')[0] as friend1,split(friends,',')[1] as friend2  from friends").show()
      //spark.sql("select  exists(array(1, 2, 3), x -> x % 2 == 0) as arr, * from characters").show()
      //spark.sql("""select name,species, height, dense_rank(height) over(partition by species order by height desc) as dr,  
      //                    rank(height) over(partition by species order by height desc) as r  
      //                   from withfriends where eyecolor = 'blue'
      //          """).show()
      spark.sql("""with 
                   s1 as (select *
                          from withfriends 
                          )
                  select *
                  from s1 
                """).show()
      spark.sql("""with 
                   s1 as (select species, collect_set(height) as hs, collect_set(name) as names, 
                          collect_list(height) as hls, collect_list(name) as nls
                          from withfriends group by species
                          )
                  select species, explode(hs) as ehs, 
                         size(hs) as sz, size(names) as sz2, size(hls) as sz3, size(nls) as sz4
                  from s1 
                """).show()
              
      spark.sql("""
                SELECT
                    from_csv(csv_string, 'id INT, name STRING, age INT', map('sep', ',')) AS parsed_csv
                FROM
                    (SELECT '1,Alice,30' AS csv_string);
                """).show()
      val sql1 = """
          WITH sales_data AS (
            SELECT 'North' AS region, 'Laptop' AS product, 100 AS sales_amount UNION ALL
            SELECT 'North' AS region, 'Tablet' AS product, 150 AS sales_amount UNION ALL
            SELECT 'South' AS region, 'Laptop' AS product, 120 AS sales_amount UNION ALL
            SELECT 'South' AS region, 'Phone' AS product, 80 AS sales_amount UNION ALL
            SELECT 'East' AS region, 'Tablet' AS product, 90 AS sales_amount 
          ),
          cube_sales AS (
            SELECT
              region,
              product,
              SUM(sales_amount) AS total_sales
            FROM
              sales_data
            GROUP BY
              CUBE(region, product)
          ),
          rollup_sales AS (
            SELECT
              region,
              product,
              SUM(sales_amount) AS total_sales
            FROM
              sales_data
            GROUP BY
              ROLLUP(region, product)
          )
          """ 
      
      spark.sql(sql1 + """
            SELECT
              'CUBE' AS operation,
              region,
              product,
              total_sales
            FROM
              cube_sales;""").show()

      spark.sql(sql1 + """
            select
            'ROLLUP' AS operation,
            region,
            product,
            total_sales
            from rollup_sales;
          """).show()        

          
        spark.sql(sql1 + """
                SELECT
                  region,
                  product,
                  SUM(sales_amount) AS total_sales
                FROM
                    sales_data
                GROUP BY
                    CUBE(region, product)
                HAVING GROUPING(region) = 1 AND GROUPING(product) = 0;
              """).show()
        spark.sql(sql1 + """
                SELECT
                  region,
                  product,
                  SUM(sales_amount) AS total_sales
                FROM
                    sales_data
                GROUP BY
                    ROLLUP(region, product)
                HAVING GROUPING(region) = 1 AND GROUPING(product) = 0;
              """).show()
              
        spark.sql(sql1 + """
                SELECT
                  region,
                  product,
                  SUM(sales_amount) AS total_sales
                FROM
                    sales_data
                GROUP BY
                    GROUPING SETS((region, product), (region), (product))
                HAVING GROUPING(region) = 1 AND GROUPING(product) = 0;
              """).show()

    }
    finally {
      spark.close()
    }
  }
}