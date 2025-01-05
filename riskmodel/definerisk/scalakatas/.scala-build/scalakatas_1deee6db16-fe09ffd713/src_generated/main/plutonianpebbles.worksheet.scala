

final class plutonianpebbles$u002Eworksheet$_ {
def args = plutonianpebbles$u002Eworksheet_sc.args$
def scriptPath = """plutonianpebbles.worksheet.sc"""
/*<script>*/
val input = "0 1 10 99 999"
val numbers = input.split(' ').map(_.toLong).toSeq



def mergeDigits(digits: Array[Long]): Long =
    digits.foldLeft(0L){
         (acc,digit) => acc * 10 + digit
    }

def SplitDigits(n: Long): Option[(Long,Long)] =
    val digits = Iterator.unfold(n) { state => {
            println(s"state $state")
            state match {
                    case 0 => None
                    case i => Some(i % 10, i / 10) // current state, new state
            }
        }
    }.toArray
    println(s"digits ${ digits.mkString(" ") } ")
    if digits.size % 2 == 0 then
        val (a,b) = digits.reverse.splitAt(digits.size / 2)
        Some((mergeDigits(a),mergeDigits(b)))
    else None

object EvenDigits {
    def unapply(n: Long): Option[(Long,Long)] = 
        SplitDigits(n)
        None
}

numbers.map { 
    n => println(n)
    val sd = SplitDigits(n)
    println(sd)
    sd
}

numbers.flatMap {
    case 0 => 1 :: Nil
    case EvenDigits(a,b) => a :: b :: Nil
    case other => other * 2024 :: Nil
}
/*</script>*/ /*<generated>*//*</generated>*/
}

object plutonianpebbles$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new plutonianpebbles$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export plutonianpebbles$u002Eworksheet_sc.script as `plutonianpebbles.worksheet`

