
import scala.collection.mutable as mut
val input3 = 
 """|AAAAAA
    |AAABBA
    |AAABBA
    |ABBAAA
    |ABBAAA
    |AAAAAA"""

val input2 = 
 """|OOOOO
    |OXOXO
    |OOOOO
    |OXOXO
    |OOOOO"""

val input1 = 
     """|RRRRIICCFF
    |RRRRIICCCF
    |VVRRRCCFFF
    |VVRCCCJFFF
    |VVVVCJJCFE
    |VVIVCCJJEE
    |VVIIICJJEE
    |MIIIIIJJEE
    |MIIISIJEEE
    |MMMISSJEEE"""

val input= 
 """|AAAA
    |BBCD
    |BBCC
    |EEEC"""

type Grid = Vector[Vector[Char]]
type Region = Vector[(Int,Int)]

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
    .linesIterator.map(line => line.toVector).toVector

def getPositions(grid: Grid) = 
    for row <- grid.indices
        col <- grid.head.indices
    yield Pos(row,col)

grid(0)(0)
val positions = getPositions(grid)
positions.toList


import scala.collection.mutable as mut

//type Region = Vector[(Int, Int)]
def cardinalPositions(x: Int, y: Int): List[(Int, Int)] = {
  List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
}

def neighborPositions(ix: Int, iy: Int): List[(Int, Int)] = {
  // look for all 8 directions 
  (ix - 1 to ix + 1).flatMap { x =>
    (iy - 1 to iy + 1).flatMap { y =>
      Option.when(x != ix || y != iy)((x, y))
    }
  }.toList
}

extension (region: Region) {
  def asPlantMap: PlantMap = {
    val maxX = region.maxBy(_._1)._1
    val maxY = region.maxBy(_._2)._2
    val res = mut.ArrayBuffer.fill(maxY + 1, maxX + 1)('.')
    region.foreach { (x, y) =>
      res(y)(x) = '#'
    }
    PlantMap(res.map(_.mkString("", "", "")).toVector)
  }
  
  def inflate: Region = {
    region.flatMap((x, y) => List((x * 2, y * 2), (x * 2 + 1, y * 2), (x * 2, y * 2 + 1), (x * 2 + 1, y * 2 + 1)))
  }

  def sides: Int = {
    val bigRegion = region.inflate
    val regionMap = bigRegion.asPlantMap
    println(regionMap)
    bigRegion.count { (x, y) =>
      val neighborCount = regionMap.optionalNeighbors(x, y).count(_.contains('#'))
      println(s"x $x y $y $neighborCount") 
      neighborCount match {
        case 3 | 4 | 7 => {
              println("*")
              true
        }
        case _ => false
      }
    }
  }

  def area: Int = region.size
  def perimeter: Int = {
    val regionMap = region.asPlantMap
    region.map((x, y) => regionMap.optionalCardinalNeighbors(x, y).count(_.forall(_ != '#'))).sum
  }
}

case class PlantMap(plants: Vector[String]) {
  val height: Int = plants.size
  val width: Int = plants.head.length
  // Length should be equal
  assert(plants.forall(_.length == width))

  def apply(x: Int, y: Int): Char = {
    plants(y)(x)
  }

  def get(x: Int, y: Int): Option[Char] = {
    Option.when(isDefinedAt(x, y))(apply(x, y))
  }

  def isDefinedAt(x: Int, y: Int): Boolean = {
    x >= 0 && x < width && y >= 0 && y < height
  }

  def indices: Vector[(Int, Int)] = {
    (for {
      y <- 0 until height
      x <- 0 until width
    } yield (x, y)).toVector
  }

  def optionalCardinalNeighbors(x: Int, y: Int): List[Option[Char]] = {
    cardinalPositions(x, y).map(get)
  }

  def optionalNeighbors(x: Int, y: Int): List[Option[Char]] = {
    neighborPositions(x, y).map(get)
  }
  
  // bfs from this position
  def floodFill(x: Int, y: Int): Region = {
    val q = mut.Queue[(Int, Int)]()
    val char = apply(x, y) // gets the char at this location
    val res = mut.ListBuffer[(Int, Int)]()  // mutable list buffer
    q.addOne((x, y))
    while (q.nonEmpty) {
      val n = q.removeHead()
      if (get(n._1, n._2).contains(char) && !res.contains(n)) {
        res.prepend(n) // if the char matches and not in the buffer add it to the top of the buffer
        q.addAll(cardinalPositions(n._1, n._2)) // add to the queue all cardinal positions add up, down, left and right from current
      }
    }
    res.toVector
  }

  // get all valid regions but we do not know for whoom
  def regions: List[Region] = {
    List.unfold[Region, Vector[(Int, Int)]](this.indices) { acc => // for every index of the grid
      acc.headOption.map { head =>
        val points = floodFill(head._1, head._2) // gives your region from the current position
        (points, acc.diff(points)) // current is region and next is all points - current points eventually process all the points
      }      
    }
  }
}


def parse(str: String): PlantMap = {
  PlantMap(str.linesIterator.toVector)
}

def part1(input: String): Int = {
  val plants = parse(input)

  plants.regions.map(r => r.area * r.perimeter).sum
}

def part2(input: String): Int = {
  val plants = parse(input)

  plants.regions.map(r => r.area * r.sides).sum
}

part1(input.stripMargin('|'))
part1(input1.stripMargin('|'))
part1(input2.stripMargin('|'))
part1(input3.stripMargin('|'))

val x = input.stripMargin('|').linesIterator.toVector
val height: Int = x.size
val width: Int = x.head.length

assert(x.forall(_.length == width))


part2(input.stripMargin('|'))