

object Element:

  private class VectorElement(
    val contents: Vector[String]
  ) extends Element

  private class LineElement(s: String) extends Element:
    val contents = Vector(s)
    override def width = s.length
    override def height = 1

  private class UniformElement(
    ch: Char,
    override val width: Int,
    override val height: Int
  ) extends Element:
    private val line = ch.toString * width
    def contents = Vector.fill(height)(line)

  def elem(contents: Vector[String]): Element =
    println(s"Vector Element ${contents}")
    VectorElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    println(s"Uniform Element ${chr}")
    UniformElement(chr, width, height)

  def elem(line: String): Element =
    println(s"Line Element ${line}")
    LineElement(line)

end Element
import Element.elem

abstract class Element:
  def contents:  Vector[String]

  def width: Int =
    if height == 0 then 0 else contents(0).length
  def height: Int = contents.length

  infix def above(that: Element): Element =
    val this1 = this.widen(that.width)
    val that1 = that.widen(this.width)
    elem(this1.contents ++ that1.contents)

  infix def beside(that: Element): Element =
    val this1 = this.heighten(that.height)
    val that1 = that.heighten(this.height)
    elem(
      for (line1, line2) <- this1.contents.zip(that1.contents)
      yield line1 + line2
    )

  def widen(w: Int): Element = 
    if w <= width then this
    else
      val left = elem(' ', (w - width) / 2, height) 
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right

  def heighten(h: Int): Element = 
    if h <= height then this
    else
      val top = elem(' ', width, (h - height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot

  override def toString = contents.mkString("\n")

end Element

object LayoutElement {
  def main(args: Array[String]): Unit = {
    
    println("example [\n" + example + "\n]")
  }

  def example = {
    val column1 = elem("hello") above elem("***")
    val column2 = elem("***") above elem("world")
    val x = column1 beside column2   
    val y = elem(Vector("one", "two","three")) beside elem(Vector("second","third","fourth"))
    x above y
  }
}

object Spiral:

  val space = elem(" ")
  val corner = elem("+")

  def spiral(nEdges: Int, direction: Int): Element =
    
    if nEdges == 1 then
      //elem("+")
      corner
    else
      val sp = spiral(nEdges - 1, (direction + 3) % 4)
      println(s"spiral edges $nEdges direction $direction")
      println(s"sp height ${sp.height} width ${sp.width}")
      def verticalBar = elem('|', 1, sp.height)
      println(s"verticalBar \n${verticalBar}")
      def horizontalBar = elem('-', sp.width, 1)
      println(s"horizontalBar\n${horizontalBar}")
      
      if direction == 0 then
        val x = (corner beside horizontalBar) above (sp beside space)
        println(s"x0\n$x")        
        x
      else if direction == 1 then
        val x = (sp above space) beside (corner above verticalBar)
        println(s"x1\n$x")
        x
      else if direction == 2 then
        val x = (space beside sp) above (horizontalBar beside corner)
        println(s"x2\n$x")
        x
      else
        val x = (verticalBar above corner) beside (space above sp)
        println(s"x3\n$x")
        x

  def main(args: Array[String]) =
    val nSides = args(0).toInt
    println(spiral(nSides, 0))

end Spiral