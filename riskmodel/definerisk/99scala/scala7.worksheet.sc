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

