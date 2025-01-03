

final class topology$u002Eworksheet$_ {
def args = topology$u002Eworksheet_sc.args$
def scriptPath = """topology.worksheet.sc"""
/*<script>*/
// scala center advent of code 2024 Day 10 tinkering

val input = 
"""|89010123
|78121874
|87430965
|96549874
|45678903
|32019012
|01329801
|10456732"""

val input2 = 
"""|0123
   |1034
   |8769
   |9876
"""

println("------------------------------------------------------------")
def reachableSummits(pos: Pos)(using graph: Map[Pos,Set[Pos]]): Set[Pos] =
    //println(s"reachableSummits $pos value ${pos.value}" )
    if pos.value == 9
    then
        println(s"9 found $pos")
        Set(pos)
    else graph(pos).flatMap(reachableSummits) 


    

type Grid = Vector[Vector[Int]]

case class Pos(row: Int, col : Int)
extension (pos: Pos) 
    def +(other: Pos): Pos = 
        Pos(pos.row + other.row,pos.col + other.col)
    def value(using Grid) =
        grid(pos.row)(pos.col)

    def IsValid(using Grid) =
        val isValidFlag = (pos.row >= 0 && pos.row < grid.length) && (pos.col >= 0 && pos.col < grid(0).length)
        isValidFlag


given grid: Grid = input.stripMargin('|')
    .linesIterator.map(line => line.toVector.map(_.asDigit)).toVector




def getPositions(grid: Grid) = 
    for row <- grid.indices
        col <- grid.head.indices
    yield Pos(row,col)


val positions = getPositions(grid)
//positions(0)



// graph has all the possible valid paths from a given position
// valid position and next value is 1 more than the current value
given graph: Map[Pos,Set[Pos]] = positions.map { pos => pos -> 
        Set(Pos(-1, 0), Pos(1, 0), Pos(0, -1), Pos(0, 1)).flatMap( offsets => Some(pos + offsets))
            .filter(nextPos => 
                nextPos.IsValid && pos.value + 1 == nextPos.value)
}.toMap

 




val starts = positions.filter(pos => pos.value == 0) // there can be multiple starts, each start has value 0
val score = starts.map(start => {
    val paths = reachableSummits(start)
    println(s"2start ${start} Paths size ${paths.size}")
    paths.foreach(println)
    paths.size
}).sum // for each start find the number of paths, the sum of all path lengths is the score
println(s"score $score")

/*</script>*/ /*<generated>*//*</generated>*/
}

object topology$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new topology$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export topology$u002Eworksheet_sc.script as `topology.worksheet`

