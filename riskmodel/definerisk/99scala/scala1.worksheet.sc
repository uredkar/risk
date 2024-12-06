import scala.reflect.ManifestFactory.NothingManifest

def last(xs: List[Int]) : Int =
    println(s"xs $xs")
    xs match 
        case last_element :: Nil => last_element
        case head :: rest => last(rest)
        case _  => throw new NoSuchElementException("empty list")

def last_but_one(xs: List[Int]) : Int =
    println(s"xs $xs")
    xs match 
        case last_but_one :: last_element :: Nil => last_but_one
        case head :: rest => last_but_one(rest)
        case _  => throw new NoSuchElementException("empty list")

extension( xs : List[Int])
    def mynth(n: Int) = 
        xs(n)

    def mylength(): Int = 
        def loop(ls: List[Int], count : Int): Int = 
            ls match 
                case Nil => count
                case _ :: tail => loop(tail,count + 1)
        loop(xs,0)        

    def mylength_fl(): Int =
        xs.foldLeft(0) {(l,_) => l + 1}

    def myrev(): List[Int] = 
        def rev(rev_ls: List[Int], ls: List[Int]) : List[Int] =
            ls match 
                case Nil => rev_ls
                case h :: tail => rev(h :: rev_ls, tail)
        rev(Nil,xs)     
    def myrev_fl(): List[Int] =
        xs.foldLeft(List[Int]()) {
            (rev_ls,h) => (h :: rev_ls) 
        }

val xs = List(1,2,3,4,5,6,7,8)
val last_element1 = last(xs)
val compare_last = last_element1 == xs.last
val last_but_one1 = last_but_one(xs)
last_but_one1 == xs.init.last
last_but_one1 == xs.takeRight(2).head
val nth = xs.mynth(4) // using extension method for fun
val l = xs.mylength()
l == xs.length
l == xs.mylength_fl()
xs.myrev()
xs.myrev_fl()
