

final class scala7$u002Eworksheet$_ {
def args = scala7$u002Eworksheet_sc.args$
def scriptPath = """.\scala7.worksheet.sc"""
/*<script>*/
import scala.annotation.tailrec

def unfold[T](init: T)(updater: T ⇒ T): Iterator[T] = new Iterator[T] {
    var current: T = init

    override def hasNext: Boolean = true

    override def next(): T = {
      val result = current
      current = updater(current)
      result
    }
}

def unfoldWhile[T](init: T)(updater: T ⇒ Option[T]): Iterator[T] = new Iterator[T] {
    var current: T = init
    var willHaveNext: Boolean = true
    override def hasNext: Boolean = willHaveNext

    override def next(): T = {
        val result = current
        updater(current) match {
        case Some(n) ⇒ current = n
        case None ⇒ willHaveNext = false
        }
        result
    }
}


val x = unfold((1,10)) {
    (a,b) => (a+1,b+2)
}
x.take(10).foreach(println)


/*</script>*/ /*<generated>*//*</generated>*/
}

object scala7$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new scala7$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export scala7$u002Eworksheet_sc.script as `scala7.worksheet`

