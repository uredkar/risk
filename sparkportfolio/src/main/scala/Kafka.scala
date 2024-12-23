import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.streaming.Trigger
import org.apache.spark.sql.functions._
import org.apache.spark.sql.expressions.Window
import org.apache.spark.sql.avro._
import org.apache.spark.sql.types._
import org.apache.kafka.clients.admin.{AdminClient, AdminClientConfig, NewTopic}
import java.util.Properties
import scala.jdk.CollectionConverters._

import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import java.util.Properties
import scala.util.Random

object KafkaProducers {
  def main(args: Array[String]): Unit = {
    val brokers = "localhost:9092" // Kafka broker address
    val stockPricesTopic = "stock-prices"
    val newsTopic = "news"

    // Configure Kafka Producer
    val props = new Properties()
    props.put("bootstrap.servers", brokers)
    props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")

    val producer = new KafkaProducer[String, String](props)

    // Simulate Stock Prices
    val stockSymbols = Seq("AAPL", "GOOGL", "MSFT", "AMZN", "TSLA")
    val random = new Random()

    def generateStockPrice(): String = {
      val symbol = stockSymbols(random.nextInt(stockSymbols.length))
      val price = 100 + random.nextDouble() * 1000
      val timestamp = System.currentTimeMillis()
      s"""{"symbol": "$symbol", "price": $price, "timestamp": $timestamp}"""
    }

    // Simulate News Articles
    val newsProviders = Seq("AP", "Reuters", "Bloomberg", "Twitter")
    def generateNews(): String = {
      val provider = newsProviders(random.nextInt(newsProviders.length))
      val content = "Breaking news about markets!"
      val timestamp = System.currentTimeMillis()
      s"""{"provider": "$provider", "content": "$content", "timestamp": $timestamp}"""
    }

    // Send Messages to Topics
    while (true) {
      // Produce stock prices
      val stockPriceRecord = new ProducerRecord[String, String](stockPricesTopic, "key", generateStockPrice())
      producer.send(stockPriceRecord)

      // Produce news
      val newsRecord = new ProducerRecord[String, String](newsTopic, "key", generateNews())
      producer.send(newsRecord)

      println(s"Produced: ${stockPriceRecord.value()} to $stockPricesTopic")
      println(s"Produced: ${newsRecord.value()} to $newsTopic")

      Thread.sleep(1000) // Simulate a delay between messages
    }

    producer.close()
  }
}



object SparkKafkaJoin {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder
      .appName("Spark Kafka Join Example")
      .config("spark.sql.warehouse.dir", "C:/tmp/spark-warehouse") // Use a valid path
      .config("spark.local.dir", "C:/tmp")
      //.config("spark.hadoop.io.nativeio.enabled", "false")
      .master("local[*]")
      .getOrCreate()

    
    spark.sparkContext.setLogLevel("ERROR")

    val stockPricesSchema = StructType(Seq(
      StructField("symbol", StringType, true),
      StructField("price", DoubleType, true),
      StructField("timestamp", TimestampType, true)
    ))

    val newsSchema = StructType(Seq(
      StructField("provider", StringType, true),
      StructField("content", StringType, true),
      StructField("timestamp", TimestampType, true)
    ))

    val stockPricesStream = spark.readStream
      .format("kafka")
      .option("kafka.bootstrap.servers", "127.0.0.1:9092")
      .option("subscribe", "stock-prices")
      .load()
      .selectExpr("CAST(value AS STRING) as json")
      .select(from_json(col("json"), stockPricesSchema).as("data"))
      .select("data.symbol", "data.price", "data.timestamp")

    val newsStream = spark.readStream
      .format("kafka")
      .option("kafka.bootstrap.servers", "127.0.0.1:9092")
      .option("subscribe", "news")
      .load()
      .selectExpr("CAST(value AS STRING) as json")
      .select(from_json(col("json"), newsSchema).as("data"))
      .select("data.provider", "data.content", "data.timestamp")

    stockPricesStream.printSchema()
    newsStream.printSchema()

    stockPricesStream.writeStream.format("console").start()
    newsStream.writeStream.format("console").start()

    val stockPricesWithWatermark = stockPricesStream.withWatermark("timestamp", "10 minutes")
    val newsWithWatermark = newsStream.withWatermark("timestamp", "10 minutes")

    // Join streams based on timestamp (this is just an example; adjust as per use case)
    //val joinedStream = stockPricesStream.join(newsStream, "timestamp")
    val joinedStream = stockPricesWithWatermark
      .join(newsWithWatermark, "timestamp") // Join on timestamp
      .select("timestamp", "symbol", "price", "provider", "content")

    val query = joinedStream.writeStream
      .outputMode("append")
      .format("console")
      .option("checkpointLocation", "/tmp/spark-checkpoint")
      .start()

    query.awaitTermination()
  }
}

object SparkJoin {
  def main(args: Array[String]): Unit = {
    //@main def SparkKafka() = {
    val spark: SparkSession = SparkSession.builder()
      .master("local[1]")
      .appName("SparkByExamples.com")
      .config("spark.sql.warehouse.dir", "file:///C:/tmp/spark-warehouse") // Use a valid path
      .config("spark.local.dir", "C:/tmp")
      .getOrCreate()

    //spark.sparkContext.setLogLevel("ERROR")
    spark.sparkContext.setLogLevel("WARN")
    import spark.implicits._

    val df = spark
      .readStream
      .format("kafka")
      .option("kafka.bootstrap.servers", "127.0.0.1:9092") // Replace with your Kafka broker address
      .option("subscribe", "my-topic") // Replace with your Kafka topic name
      .option("group.id", "scala-consumer-group")
      .option("startingOffsets", "earliest") // Start reading from earliest messages
      .load()

    val valueDF = df.selectExpr("CAST(value AS STRING)")

    // Process the data (example: count words)
    val wordCounts = valueDF
      .select(explode(split($"value", " ")).as("word"))
      .groupBy("word")
      .count()

    // Write the results to console
    val query = wordCounts.writeStream
      .outputMode("complete")
      .format("console")
      .trigger(Trigger.ProcessingTime("10 seconds")) // Trigger every 10 seconds
      .start()
    query.awaitTermination()
  }
}


import org.apache.kafka.clients.consumer.{ConsumerConfig, KafkaConsumer}
import java.util.Properties
import scala.jdk.CollectionConverters._

object KafkaScalaConsumer {
  def main(args: Array[String]): Unit = {
    val topic = "stock-prices" // Replace with your topic

    // Kafka consumer properties
    val props = new Properties()
    props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "127.0.0.1:9092") // Update as needed
    props.put(ConsumerConfig.GROUP_ID_CONFIG, "scala-consumer-group")
    props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer")
    props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer")
    props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")

    val consumer = new KafkaConsumer[String, String](props)

    // Subscribe to the topic
    consumer.subscribe(java.util.Arrays.asList(topic))

    println(s"Subscribed to topic: $topic")

    try {
      while (true) {
        val records = consumer.poll(java.time.Duration.ofMillis(1000))

        for (record <- records.asScala) {
          println(s"Received message: Key = ${record.key()}, Value = ${record.value()}, Partition = ${record.partition()}, Offset = ${record.offset()}")
        }
      }
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      consumer.close()
    }
  }
}


import java.util.Properties
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}

object KafkaProducerTest {
  def main(args: Array[String]): Unit = {
    val props = new Properties()
    props.put("bootstrap.servers", "localhost:9092") // External port
    props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    
    val producer = new KafkaProducer[String, String](props)
    val topic = "my-topic"

    try {
      val record = new ProducerRecord[String, String](topic, "key", "value")
      producer.send(record)
      println("Message sent successfully!")
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      producer.close()
    }
  }
}



