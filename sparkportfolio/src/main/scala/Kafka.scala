import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.streaming.Trigger
import org.apache.spark.sql.functions._
import org.apache.spark.sql.expressions.Window
import org.apache.spark.sql.avro._


@main def SparkKafka() = {
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


import org.apache.kafka.clients.consumer.{ConsumerConfig, KafkaConsumer}
import java.util.Properties
import scala.jdk.CollectionConverters._

object KafkaScalaConsumer {
  def main(args: Array[String]): Unit = {
    val topic = "my-topic" // Replace with your topic

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
