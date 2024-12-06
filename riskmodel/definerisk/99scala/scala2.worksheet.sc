//https://aperiodic.net/pip/scala/s-99/
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

    def myrev_fl(): List[Int] =
        xs.foldLeft(List[Int]()) {
            (acc,h) => (h :: acc) 
        }
    def is_palindrome(): Boolean =
        xs == xs.myrev_fl()        
    
    def is_palindrome2(): Boolean =
        def loop(index: Int): Boolean =
            println(s"index $index")
            if index >= xs.length || xs.length-index-1 < 0 then // check out of bounds
                true
            else 
                val h1 = xs(index)
                val h2 = xs(xs.length-index-1)
                if h1 == h2 then
                    loop(index + 1)
                else
                    false
                end if 
            end if 
            
        loop(0)

    def compress1() : List[Int] =
        xs.foldRight(List[Int]()) {
            (r,acc) => {
                if (acc.isEmpty || acc.head != r) 
                    r :: acc 
                else 
                    acc
            }
        }

    def compress2() : List[Int] =
        xs.foldLeft(List[Int]()) {
            (acc,r) => {
                if (acc.isEmpty || acc.head != r ) 
                    r :: acc
                else 
                    acc
            }
        }.reverse

    def compress3() : List[Int] =
        def compress(r: List[Int],ls : List[Int]): List[Int] = 
            ls match 
                case h :: tail => compress(h :: r,tail.dropWhile(_ == h))
                case Nil => r
        compress(Nil,xs).reverse

    def pack() : List[List[Int]] = 
        def pack_inner(ls: List[Int]): List[List[Int]] = 
            if (ls.isEmpty) List(List())
            else {
                val (packed, next) = ls span {
                    _ == ls.head
                }
                if (next == Nil) 
                    List(packed)
                else 
                    packed :: pack_inner(next)
            }
        pack_inner(xs)

    def encode() : List[(Int,Int)] =
        xs.pack() map { e =>  (e.length, e.head)}
        
extension( xxs : List[List[Int] | Int])
    def myflatmap() = 
            xxs.flatMap {
            case i: Int => List(i)
            case l: List[Int] => l
        }




val xs = List(1,2,3,4,5,6,7,8,9)
xs.is_palindrome()
xs.is_palindrome2()
val ys = List(1,2,3,2,1)
ys.is_palindrome()
ys.is_palindrome2()
List().is_palindrome2()
List(1).is_palindrome2()
List(1,1).is_palindrome2()
List(1,2).is_palindrome2()

val xxs = List(1,2,List(3,4,5,6),7,8, List(9,10),11)
xxs.myflatmap()
List(1,2,2,3,3,4,5,6,7,7,7).compress1()
List(1,2,2,3,3,4,5,6,7,7,7).compress2()
List(1,2,2,3,3,4,5,6,7,7,7).dropWhile(p =>{ 
                println(s"drop while $p")
                p != 7
            }) 

List(1,2,2,3,3,4,5,6,7,7,7).compress3()
val (f,s) = List(1,2,2,3,3,4,5,6,7,7,7).span{ p => p  == 2}
println(s"f $f s $s")
List(1,2,2,3,3,4,5,6,7,7,7).pack()
val l = List(0,1,1, 9, 8, 0,1)
l.span(e => e < 2)
List(1,2,2,3,3,4,5,6,7,7,7).encode()