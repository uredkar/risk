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


def unfold2[A, B](f: A => (A, B))(seed: A): LazyList[B] =
  LazyList.unfold(seed) { current =>
    val (nextSeed, result) = f(current)
    Some((result, nextSeed))
  }



// Example usage:
val xis = unfold2[(Int, Int), (Int, String)] {
  case (a, b) => ((a + 1, b + 2), (a, (b + 2).toString))
}

val xii = unfold2[(Int, Int), (Int, Int)] {
  case (a, b) => ((a + 1, b + 2), (a, (b + 2)))
}

val xsii = xii(1,10).take(5).toList 
//println(xsii)

val xsis = xis(1,10).take(5).toList 
//println(xsis)

val x1 = xis(2,12)
val xs1 = x1.take(5).toList
//println(xs1)
val xxs = List(xs1,xsis)
println(xxs)
val fa1 = xxs.flatMap(a => a).flatMap((a,b) => List(a,b))
println(s"list of list fm $fa1")
val a = 1 to 3
val m0 = a.map(j => (j.toString,j))
val m1 = a.map(j => (j,j*j))
println(s"m1 $m1")
val ft = (1,2,3).toList
val fm1 = m1.flatMap((a,b) => List(a,b))
println(s"fm1 $fm1")
val fm2 = a.flatMap(i => a.map(j => (i,j) -> i*j)).toMap
println(fm2)


  
