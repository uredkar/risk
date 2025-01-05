

final class restroomrobot$u002Eworksheet$_ {
def args = restroomrobot$u002Eworksheet_sc.args$
def scriptPath = """restroomrobot.worksheet.sc"""
/*<script>*/
// advent scala center 2024 day 14

case class Vec2i(x: Int, y: Int)



def parse(str: String): List[Robot] =
  str.linesIterator.map:
    case s"p=$px,$py v=$vx,$vy" =>
      Robot(Vec2i(px.toInt, py.toInt), Vec2i(vx.toInt, vy.toInt))
  .toList


val size = Vec2i(101, 103)

extension (self: Int)
  infix def rem(that: Int): Int =
    val m = math.abs(self) % that
    if self < 0 then
      that - m
    else
      m

case class Robot(pos: Vec2i, velocity: Vec2i) {
  def stepN(n: Int = 1): Robot =
    copy(pos = pos.copy(x = (pos.x + n * velocity.x) rem size.x, y = (pos.y + n * velocity.y) rem size.y))
}


extension (robots: List[Robot])
  def show(): String =
    val blank = Array.fill(size.y+1)(Array.fill(size.x+1)('.'))
    for
      robot <- robots
      (y, x) = (robot.pos.y,robot.pos.x)
    do blank(y)(x) = '#'
    blank.map(_.mkString).mkString("\n")

  def stepN(n: Int = 1): List[Robot] = robots.map(_.stepN(n))

  def safety: Int =
    val middleX = size.x / 2
    val middleY = size.y / 2

    robots.groupBy: robot =>
      (robot.pos.x.compareTo(middleX), robot.pos.y.compareTo(middleY)) match
        case (0, _) | (_, 0) => -1
        case ( 1, -1) => 0
        case (-1, -1) => 1
        case (-1,  1) => 2
        case ( 1,  1) => 3
    .removed(-1).values.map(_.length).product

  def findEasterEgg: Int =
    (0 to 10403).find { i =>
      val newRobots = robots.stepN(i)
      newRobots.groupBy(_.pos.y).count(_._2.length >= 10) > 15 && newRobots.groupBy(_.pos.x).count(_._2.length >= 15) >= 3
    }.getOrElse(-1)



def part1(input: String): Int = parse(input).stepN(100).safety

def part2(input: String): Int = parse(input).findEasterEgg

import java.io.File
val currentDir = new File(".").getCanonicalPath
val file = new File(currentDir ,"day14.input")
//C:\sources\risk\riskmodel\definerisk\scalakatas\day14.input
val inp = scala.io.Source.fromFile(file).mkString

val step1 = part1(inp)
val step2 = part2(inp)
val p = parse(inp).stepN(step2)
println(p.show())
/*</script>*/ /*<generated>*//*</generated>*/
}

object restroomrobot$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new restroomrobot$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export restroomrobot$u002Eworksheet_sc.script as `restroomrobot.worksheet`

