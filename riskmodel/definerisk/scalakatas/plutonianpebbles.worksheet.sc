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
        SplitDigits(n) // if this is none then no match else match
        
}



def blink(stones: Seq[Long]): Seq[Long] =
    stones.flatMap {
        case 0 => 1 :: Nil
        case EvenDigits(a,b) => a :: b :: Nil // unaply is applied in case
        case other => other * 2024 :: Nil
    }

numbers.sum
val stonesTest = Iterator
    .iterate(List(125L,17L))(blink)
    .drop(25)
    .next()

stonesTest.size

val numbers2 = "0 5601550 3914 852 50706 68 6 645371".split(' ').map(_.toLong).toSeq


val stones = Iterator
    .iterate(numbers2)(blink)
    .drop(25)
    .next()

stones.size
