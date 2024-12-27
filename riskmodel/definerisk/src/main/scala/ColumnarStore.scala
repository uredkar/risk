package com.columar
import scala.util.Random    
import upickle.default.{*,given}
import java.io.{File, PrintWriter}
import scala.io.Source

// Case class to represent a single row
case class Row(name: String, age: Int, partitionKey: String)

// Case class to represent columnar data
case class ColumnarData(name: List[String], age: List[Int], partitionKey: List[String])

// Companion object for serialization/deserialization
object ColumnarData:
  implicit val rw: ReadWriter[ColumnarData] = macroRW

// ColumnarStore class
class ColumnarStore(filePath: String):
  // Save data to file in columnar format
    def save(data: ColumnarData): Unit =
        val json = write(data)
        val writer = new PrintWriter(new File(filePath))
        try writer.write(json) finally writer.close()

  // Load data from file in columnar format
    def load(): Option[ColumnarData] =
        val file = new File(filePath)
        if file.exists() && file.length() > 0 then
            val source = Source.fromFile(file)
            try Some(read[ColumnarData](source.mkString))
            finally source.close()
        else None

  // Add a new partition (append new rows)
    def addPartition(newRows: List[Row]): Unit =
        val existingData = load().getOrElse(ColumnarData(Nil, Nil, Nil))
        val updatedData = ColumnarData(
        name = existingData.name ++ newRows.map(_.name),
        age = existingData.age ++ newRows.map(_.age),
        partitionKey = existingData.partitionKey ++ newRows.map(_.partitionKey)
        )
        save(updatedData)

    // Delete a partition based on a predicate
    
    def filterByPredicate(predicate: (String, Int, String) => Boolean): ColumnarData = {
        val existingData = load().getOrElse(ColumnarData(Nil,Nil,Nil))
        val filteredIndices = existingData.name.indices.filter(i => predicate(existingData.name(i), existingData.age(i), existingData.partitionKey(i)))

        ColumnarData(
            existingData.name.zipWithIndex.filter { case (_, index) => filteredIndices.contains(index) }.map(_._1),
            existingData.age.zipWithIndex.filter { case (_, index) => filteredIndices.contains(index) }.map(_._1),
            existingData.partitionKey.zipWithIndex.filter { case (_, index) => filteredIndices.contains(index) }.map(_._1)
        )
    }

  
    def deletePartitionWithPredicate(predicate: (String, Int, String) => Boolean): Unit =
        val existingData = load().getOrElse(ColumnarData(Nil, Nil, Nil))
        val filteredIndices = existingData.name.indices.filter(i => predicate(existingData.name(i), existingData.age(i), existingData.partitionKey(i)))
        
        val updatedData = ColumnarData(
                existingData.name.zipWithIndex.filter { case (_, index) => !filteredIndices.contains(index) }.map(_._1),
                existingData.age.zipWithIndex.filter { case (_, index) => !filteredIndices.contains(index) }.map(_._1),
                existingData.partitionKey.zipWithIndex.filter { case (_, index) => !filteredIndices.contains(index) }.map(_._1)
            )
        save(updatedData)
    
    def deletePartition(partitionKey: String): Unit =
        val existingData = load().getOrElse(ColumnarData(Nil,Nil,Nil))
        val filteredIndices = existingData.partitionKey.indices.filter(i => existingData.partitionKey(i) != partitionKey)
        //val filteredIndices = data.name.indices.filter(i => data.name(i) == filterName)
        val updatedData = ColumnarData(
                existingData.name.zipWithIndex.filter { case (_, index) => filteredIndices.contains(index) }.map(_._1),
                existingData.age.zipWithIndex.filter { case (_, index) => filteredIndices.contains(index) }.map(_._1),
                existingData.partitionKey.zipWithIndex.filter { case (_, index) => filteredIndices.contains(index) }.map(_._1)
            )
        save(updatedData)


  // Read specific columns
    def readColumns(columns: List[String]): Map[String, List[Any]] =
        val existingData = load().getOrElse(ColumnarData(Nil, Nil, Nil))
        val columnarMap = Map(
        "name" -> existingData.name,
        "age" -> existingData.age,
        "partitionKey" -> existingData.partitionKey
        )
        columnarMap.filter { case (key, _) => columns.contains(key) }


def randomCity(cities: List[String]): String = {
  val randomIndex = Random.nextInt(cities.length)
  cities(randomIndex)
}

// Example usage:
val cities = List("New York", "London", "Paris", "Tokyo", "Sydney")
//val randomCityName = randomCity(cities)



// Generate random rows
def generateRandomRows(n: Int): List[Row] =
  (1 to n).map { i =>
    Row(
      name = s"Person_$i",
      age = Random.between(18, 65),
      partitionKey = randomCity(cities)
    )
  }.toList

// Usage example
@main def runColumnarStore() =
    val store = ColumnarStore("columnar_store.json")

    // Initial rows

    val rowsCustom = List(
        Row("Alice", 25,"London"),
        Row("Bob", 30,"London"),
        Row("Charlie", 35,"London")
    )
  

    val rows = generateRandomRows(10) ++ rowsCustom
    println("------------------------------")
    println(rows)
    println("------------------------------")
    // Save data to columnar store
    store.addPartition(rows)

    // Add a new partition
    store.addPartition(List(Row("David", 40,"Paris")))

    println("----age > 10 and Alice --------------------------")
    val filteredData = store.filterByPredicate((name, age, _) => age > 10 && name == "Alice") 
    println(filteredData) // Output: ColumnarData(List(Alice, Alice),List(30, 30),List(A, A))
    println("-------age > 10 and London -----------------------")
    // Filter by age > 10 and partitionKey = "A"
    val filteredData2 = store.filterByPredicate((_, age, partitionKey) => age > 10 && partitionKey == "London") 
    println(filteredData2) // Output: ColumnarData(List(Alice, Alice),List(30, 30),List(A, A))

    // Delete a partition where age > 30

    //store.deletePartition(row => row.age > 30)
    println("Delete -------(age > 10 and Alice) or Toyko -----------------------")
    store.deletePartitionWithPredicate((name, age, _) => age > 30 || name == "Person_10")
    store.deletePartition("Paris")
    // Read specific columns
    val columns = store.readColumns(List("name","age","partitionKey"))
    println(s"Read columns: $columns")
