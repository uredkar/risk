import com.definerisk.core.utils.PrettyPrinter.{*, given}
import scala.compiletime.summonFrom
import scala.reflect.ClassTag
import com.definerisk.core.models.*
import com.definerisk.core.OptionStrategies.*
import java.time.LocalDate

@main def strategyApp(): Unit =
  // Example usage
  given Underlying = Underlying("AAPL", 150.00, LocalDate.now)
  val coveredCallAnalysis = CoveredCallWithAnalysis(coveredCall(155.00, 3.00, LocalDate.now.plusMonths(1)), 3.00, 0.0)

  // Implicitly provide the underlying context
  //given Underlying = Underlying("AAPL", 150.00, LocalDate.now)
  given PrettyPrinter[OptionsStrategy] with
    def prettyPrint(strategy: OptionsStrategy): String =
      s"OptionsStrategy: ${strategy.legs.map(_.toString).mkString(", ")}"

  // Given instance for a generic list of strategies, if needed
  given PrettyPrinter[List[OptionsStrategy]] with
    def prettyPrint(strategies: List[OptionsStrategy]): String =
      strategies.map(_.pretty).mkString("\n")

  // Define strategies
  val coveredCallStrategy = coveredCall(155.00, 3.00, LocalDate.now.plusMonths(1))
  val longStraddleStrategy = straddle(150.00, 5.00, 4.00, LocalDate.now.plusMonths(1),true)
  val shortStraddleStrategy = straddle(150.00, 5.00, 4.00, LocalDate.now.plusMonths(1),false)
  val bullCallSpreadStrategy = bullCallSpread(150.00, 160.00, 4.00, 2.00, LocalDate.now.plusMonths(1))

  // Pretty print strategies
  printPretty(coveredCallStrategy)
  printPretty(longStraddleStrategy)
  printPretty(bullCallSpreadStrategy)

