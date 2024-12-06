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
    
    def encodeModified(): List[Any] = 
        xs.encode() map {
            t => {
                if (t._1 == 1) t._2 else t
            }
        }
    
    def encodeDirect(): List[(Int,Int)] = 
        if (xs.isEmpty) Nil
        else {
            val (packed, next ) = xs.span { _ == xs.head }
            (packed.length, packed.head) :: next.encodeDirect()
        }

    def duplicate(): List[Int] =
        xs.foldLeft(List[Int]()) {
            (acc,r ) => r :: r :: acc
        }.reverse

    def duplicate(n: Int): List[Int] =
        xs.foldLeft(List[Int]()) {
            (acc,r ) =>  List.fill(n)(r) ::: acc
        }.reverse
                
        
extension( xxs : List[List[Int] | Int])
    def myflatmap() = 
            xxs.flatMap {
            case i: Int => List(i)
            case l: List[Int] => l
        }

extension(xxs : List[(Int,Int)])
    /**
        * returns decoded for encode values
        val encoded = List(1,2,2,3,3,4,5,6,7,7,7).encode()
        encoded.decode()
        encoded.decode2()
        encoded.decode() == encoded.decode2()
        
        */
    def decode(): List[Int] =
        xxs.flatMap { e => List.tabulate(e._1) (n => e._2) }
            
    def decode2(): List[Int] =
        xxs.flatMap { e => List.fill(e._1)(e._2) }

        




val xs = List(1,2,3,4,5,6,7,8,9)
xs.pack()
xs.encode()
xs.encodeModified()

val encoded = xs.encode()
encoded.decode()
encoded.decode2()
encoded.decode() == encoded.decode2()
val encodedDirect = List(1,2,2,3,3,4,5,6,7,7,7).encodeDirect()
encodedDirect == encoded
xs.duplicate()
xs.duplicate(2)
xs.duplicate(3)