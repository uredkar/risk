import scala.util.Try
import scala.math.Fractional.Implicits.infixFractionalOps
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Numeric.Implicits.infixNumericOps
import scala.util.Using
import java.io.PrintWriter
import com.definerisk.core.models.{*,given}
case class AccountData(accountName: String, grid: Grid)


trait Parsable[T]:
  def parse(value: String): Option[T]

given Parsable[Double] with
  def parse(value: String): Option[Double] = value.toDoubleOption

given Parsable[Int] with
  def parse(value: String): Option[Int] = value.toIntOption

given Parsable[String] with
  def parse(value: String): Option[String] = Some(value)

def parseValue[T: Parsable](value: String): Option[T] =
  summon[Parsable[T]].parse(value)


type Parse[T] = T match
  case Double => Option[Double]
  case Int    => Option[Int]
  case String => Option[String]
  case _      => Option[Nothing]

case class CSVRow(headers: List[String], columns: Map[String, String])

trait Summarizable[T]:
  def summarize(values: List[T]): T

given Summarizable[Double] with
  def summarize(values: List[Double]): Double = values.sum

given Summarizable[Int] with
  def summarize(values: List[Int]): Int = values.sum


case class Grid(headers: Vector[String], rows: Vector[Vector[String]]) {
    def getRow(index: Int): Option[Vector[String]] = rows.lift(index)

    def filterRows(predicate: Vector[String] => Boolean): Grid = 
        Grid(headers, rows.filter(predicate))

    def groupBy[T: Numeric](keyColumn: String, valueColumn: String)(convert: String => T): Map[String, T] = {
        val keyIndex = headers.indexOf(keyColumn)
        val valueIndex = headers.indexOf(valueColumn)

        val numeric = implicitly[Numeric[T]]
        import numeric._

        rows.groupBy(row => row(keyIndex))
        .view.mapValues(group =>
            group.map(row => convert(row(valueIndex))).sum
        ).toMap
    }
}

import scala.collection.mutable


def extractCDInfo(cdInfo: String): (Option[Double], Option[String]) = {
    val rateRegex = """(\d+\.\d+)%""".r
    val dateRegex = """DUE\s+(\d{2}/\d{2}/\d{2})""".r

    val interestRate = rateRegex.findFirstMatchIn(cdInfo).map(_.group(1).toDouble)
    val dueDate = dateRegex.findFirstMatchIn(cdInfo).map(_.group(1))

    (interestRate, dueDate)
}


def calculateCDTotal(
      rows: Seq[Seq[String]],
      descriptionIndex: Int,
      marketValueIndex: Int
  ): Double = {
    //println("descriptionIndex " + descriptionIndex)
    //println("marketValueIndex " + marketValueIndex)
    rows.foldLeft(0.0) { (total, row) =>
      
      if (descriptionIndex >= 0 && row.isDefinedAt(descriptionIndex) &&
          row(descriptionIndex).toLowerCase.contains("cd fdic ins") &&
          marketValueIndex >= 0 && row.isDefinedAt(marketValueIndex)) {
        try {
          
          //println(s"calculateCDTotal row value ${ row(marketValueIndex).parseNumericValue} -> ${row(descriptionIndex)}")
          //println(s"calculateCDTotal row $row")
          total + row(marketValueIndex).parseNumericValue
        } catch {
          case e: Exception => {
                    //NumberFormatException => total // Ignore rows with invalid market values
                    println(s"An unexpected exception occurred: ${e.getMessage}") // Catch-all for other exceptions
                    total
          }
    
        }
      } else {
        total
      }
    }
  }

  implicit class StringParsing(val s: String) {
    def parseNumericValue: Double = {
      try {
        s.replaceAll("[^\\d.]", "").toDouble
      } catch {
        case _: NumberFormatException => 0.0
      }
    }
  }
def printSummaryResults(accountTotals: mutable.Map[String, Double], accountCDTotals: mutable.Map[String, Double]): Unit = {
    val cdAllAccountTotal = accountCDTotals.values.sum
    val allAccountTotals = accountTotals.values.sum
    accountTotals.foreach { case (accountName, total) =>
        val cdTotal = accountCDTotals.getOrElse(accountName, 0.0)
        val percentage = if (total > 0) (cdTotal / total) * 100 else 0.0
        
        println(s"Summary Account: $accountName, Total: %.2f, CD Total: %.2f, CD Percentage: %.2f".format(total, cdTotal, percentage))
    

    }
    println(s"Summary Total: %.2f, CD Total: %.2f, CD Percentage: %.2f".format(allAccountTotals, cdAllAccountTotal, (cdAllAccountTotal /allAccountTotals) * 100))
}
// Define the AccountData and Grid classes as before
//case class Grid(headers: Vector[String], rows: Vector[Vector[String]])
//case class AccountData(accountName: String, grid: Grid)
def calculateMarketValuePercentage(accountData: Vector[AccountData]):  Vector[AccountData] = {
    // Calculate the portfolio total market value
    val portfolioTotal = accountData.flatMap { account =>
        val mktValIndex = account.grid.headers.indexOf("Mkt Val (Market Value)")
        account.grid.rows.map(row => row(mktValIndex).parseNumericValue)
    }.sum

    println(s"Portfolio Total Market Value: $$${portfolioTotal}")

    // Map to hold account totals for easy lookup
    val accountTotals = mutable.Map[String, Double]()
    val accountCDTotals = mutable.Map[String, Double]()

    // Iterate over each account and calculate percentages
    val updatedAccounts = accountData.zipWithIndex.map { case (account,index) =>
        val headers = account.grid.headers
        val rows = account.grid.rows
        val mktValIndex = headers.indexOf("Mkt Val (Market Value)")
        val descrIndex = headers.indexOf("Description")
        val accountCDTotal = calculateCDTotal(rows, descrIndex, mktValIndex)

        //println(s"Account: ${account.accountName} CD Total: $$${accountCDTotal}")

        // Calculate account total market value
        val accountTotal = rows.map(row => row(mktValIndex).parseNumericValue).sum
        accountTotals(account.accountName) = accountTotal
        accountCDTotals(account.accountName) = accountCDTotal
        println("\n-------------------------------------------------------------------------------------------------------------")
        println(s"${account.accountName} Total Market Value: $$${accountTotal} CD Total: $$${accountCDTotal}")

        // Add new percentage columns
        val updatedHeaders = headers :+ "% of Account" :+ "% of Portfolio"
        val updatedRows = rows.map { row =>
            val mktVal = row(mktValIndex).parseNumericValue
            print(s"\n${row.head} mktVal $mktVal ")
            extractCDInfo(row(descrIndex)) match {
                case (Some(rate), Some(dueDate)) => print(s" CD Rate: $rate, Due Date: $dueDate")
                case _ => // do nothing
            }
            
            val percentageOfAccount = (mktVal / accountTotal) * 100
            val percentageOfPortfolio = (mktVal / portfolioTotal) * 100
            row :+ f"$percentageOfAccount%.2f" :+ f"$percentageOfPortfolio%.2f"
        }
        // Add the "Account Total" line
        val accountTotalRow = Vector.fill(mktValIndex - 1)(" ") :+ "Total" :+ 
                      f"$$${accountTotal}%.2f" :++
                      Vector.fill(headers.size - mktValIndex + 1)(" ")
        //println(s"------------- accountTotalRow $accountTotalRow accountRowSize ${accountTotalRow.size} header size ${headers.size} mktValIndex $mktValIndex")

        // Return updated account data
        
        
        if (index == accountData.size - 1) {
            //println("Last index about to add portfolio total")
            val portfolioTotalRow = Vector.fill(mktValIndex - 1)(" ") :+ "Portfolio Total" :+ 
                    f"$$${portfolioTotal}%.2f" :++
                    Vector.fill(headers.size - mktValIndex + 1)(" ")
            //println(s"------------- portfolioTotalRow $portfolioTotalRow portfolioTotalRow ${portfolioTotalRow.size} header size ${headers.size} mktValIndex $mktValIndex")
            AccountData(account.accountName, Grid(updatedHeaders, updatedRows :+ accountTotalRow :+ portfolioTotalRow))                    
            //AccountData(account.accountName, Grid(updatedHeaders, updatedRows :+ accountTotalRow))
        }
        else {
            AccountData(account.accountName, Grid(updatedHeaders, updatedRows :+ accountTotalRow))
        }
        
        /*
        val rowsWithTotals = updatedRows :+ accountTotalRow
        rowsWithTotals.zipWithIndex.foreach { case (row, rowIndex) =>
                println(s"Row $rowIndex: ${row.mkString(", ")}") // Print the entire row
  
                // Access individual columns of the row
                row.zipWithIndex.foreach { case (colValue, colIndex) =>
                println(s"Column $colIndex: $colValue")
            }
        }
            */
        //AccountData(account.accountName, Grid(updatedHeaders, updatedRows :+ accountTotalRow))   
    }
    
    printSummaryResults(accountTotals, accountCDTotals)
    println("\n-------------------------------------------------------------------------------------------------------------")
    updatedAccounts
}


object CSVParser {
    def parseCSVLine(line: String): Vector[String] = {
        val regex = """(?<=^|,)(?:"([^"]*)"|([^",]*))(?:,|$)""".r
        regex.findAllMatchIn(line).map { m =>
            Option(m.group(1)).getOrElse(m.group(2)) // Extract quoted or unquoted value
        }.toVector
    }

    def parseCSVSpliter(csv: String): Vector[Vector[String]] = {
          csv.split("\n")
            .toVector
            .map(_.trim)
            .filter(_.nonEmpty)
            .map(parseCSVLine)
    }

    def parseCSV(csv: String): Vector[AccountData] = {
        //val lines = csv.split("\n").toVector.map(_.split(",").map(_.trim).toVector)
        //println(s"Parsed lines: ${lines.mkString("\n")}") // Debugging output
        //val lines = csv.split("\n").toVector.map(_.trim).filter(_.nonEmpty).map(_.split(",").map(_.trim.stripPrefix("\"").stripSuffix("\"")).toVector)
        val lines = parseCSVSpliter(csv)
        //println(s"Parsed lines:\n${lines.mkString("\n")}") // Debugging output
        val accountData = collection.mutable.ArrayBuffer.empty[AccountData]

        var currentAccount: Option[String] = None
        var headers: Vector[String] = Vector.empty
        var rows: Vector[Vector[String]] = Vector.empty

        lines.foreach {
            case line if line.head == "Account Total" || line.head.startsWith("Position for") => 
            case line if line.forall(_.isEmpty) => // Skip empty lines
                //println("Skipping empty line") // Debugging output
            case line  if line.head.nonEmpty && line.tail.forall(_.isEmpty) =>
                // Save the previous account data if any
                if (currentAccount.isDefined && headers.nonEmpty && rows.nonEmpty) {
                    //println(s"Saving account data for: ${currentAccount.get}")
                    accountData.append(AccountData(currentAccount.get, Grid(headers, rows)))
                }
                //println("start new account")
                // Start a new account
                currentAccount = Some(line.head)
                headers = Vector.empty
                rows = Vector.empty
            case line if line.head == "Symbol" => // Header row
                //println(s"Found headers: ${line.mkString(", ")}") // Debugging output
                headers = line
            case line if headers.nonEmpty  => // Data row
                //println(s"Adding row: ${line.mkString(", ")}") // Debugging output
                rows = rows :+ line
            case other => // Ignore other lines
                println(s"Ignored line: ${other.mkString(", ")}") // Debugging output
        }

        // Save the last account data
        if (currentAccount.isDefined && headers.nonEmpty && rows.nonEmpty) {
            //println(s"Saving last account data for: ${currentAccount.get}")
            accountData.append(AccountData(currentAccount.get, Grid(headers, rows)))
        }

        accountData.toVector
    }
}

extension (value: String)
    def parseNumericValue: Double = {
        val cleanedValue: String = value.trim match {
        case s if s.startsWith("-$") => "-" + s.substring(2).replace(",", "")
        case s if s.startsWith("$") => s.substring(1).replace(",", "")  // Remove $ and commas
        case s if s.endsWith("%") => s.dropRight(1).replace(",", "")  // Convert percentage
        case s if s == "N/A" => "0.0"
        case s => s.replace(",", "")  // Just remove commas
        
        }

        // Try parsing the cleaned value as a Double
        //Try(cleanedValue.toDouble).toOption
        cleanedValue.toDouble
    }

@main def testNumericValues() : Unit =
    
    val csvData = List(
      "1,000", "$12.1744", "-2.6%", "-$0.3256", "$12174.4", "-$325.6", "-2.6%", "$10245.8", 
      "18.82%", "$1928.6", "2.09%", "N/A", "-21.57","$11710.16","$10,000"
    )

    val parsedData = csvData.map(parseNumericValue)

    parsedData.foreach {
      value => println(s"Parsed value: {$value}")
      
    }
  
def renderTable(account: AccountData): String = {
  val headers = account.grid.headers
  val rows = account.grid.rows

  // Calculate column widths
  val columnWidths = (headers +: rows).transpose.map(_.map(_.length).max)

  // Format a single row
  def formatRow(row: Vector[String]): String = 
    row.zip(columnWidths).map { case (cell, width) => cell.padTo(width, ' ') }.mkString(" | ")

  // Combine header, rows, and separators
  val separator = columnWidths.map("-" * _).mkString("-+-")
  val table = (formatRow(headers) +: separator +: rows.map(formatRow)).mkString("\n")

  s"Account Name: ${account.accountName}\n$table"
}
@main def testcsvparser(): Unit =
    val source = scala.io.Source.fromFile(ConfigReader.dataFileLocation)
    val accounts = CSVParser.parseCSV(source.mkString)
    source.close()
    /*
    accounts.foreach { account =>
        println(s"Account: ${account.accountName}")
        println(s"Headers: ${account.grid.headers}")
        //account.grid.rows.foreach(row => println(s"Row: $row"))
        // Calculate and display the results
        
    }
        */
    val updatedAccounts = calculateMarketValuePercentage(accounts)
    import pprint._

    //pprint.pprintln(accounts)
    //val table = accountData.grid.headers +: accountData.grid.rows
    //println(table.toTable.toString)
    val writer = new PrintWriter(ConfigReader.outputDirectory)
    

    updatedAccounts.foreach { account =>
        //println(s"account.grid.rows.size()")
        writer.println(renderTable(account))
    }
    writer.close()

