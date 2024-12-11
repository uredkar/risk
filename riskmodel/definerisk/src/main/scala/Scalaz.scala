import scalaz._
import Scalaz._

 



@main def testscalaz() =
    
    val x = 3 |+| 4
    println(s"x $x")
    val y = 3.show
    println(y)
    ('a' to 'e').foreach(println)