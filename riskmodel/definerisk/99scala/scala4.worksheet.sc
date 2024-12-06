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
                
    def drop_every_nth(n: Int): List[Int] =
        xs.zipWithIndex.filter {
            e => (e._2 + 1) % n != 0
        }.map { _._1 } 
            

    def drop_every_nth_2(n: Int): List[Int] = {
        xs.grouped(n).flatMap(group => group.take(n - 1)).toList
    }        

    def drop_every_nth_3(n: Int): List[Int] = {
        xs.indices.collect {
            case i if (i + 1) % n != 0 => xs(i) }.toList
        
    }

    def drop_every_nth_4(n: Int) : List[Int] = {
        var count = 1
        for ( e  <- xs if {
            val keep = count % n != 0; count += 1; keep
        }) yield e
    }
            
    def drop_every_nth_5(n: Int) : List[Int] =    
        xs.sliding(n-1,n).flatten.toList

    def split(n: Int) : (List[Int],List[Int]) = 
        (xs.take(n),xs.drop(n))

    def split_2(n: Int) : (List[Int],List[Int]) = 
        xs.splitAt(n)

    def rotate(n: Int = 3): List[Int] =
        val nBound = if (xs.isEmpty) 0 else n % xs.length
        if (nBound < 0) rotate(nBound + xs.length)
        else (xs.drop(nBound)) ::: xs.take(nBound)

    def removeAt(range: Range): List[Int] = 
        val (left, right) = xs.splitAt(range.start)
        left ++ right.drop(range.size)
    
    def removeAt_2(range: Range): List[Int] = 
        xs.zipWithIndex.filterNot { case (_, idx) => range.contains(idx) }.map(_._1)
    

    def getAt_2(range: Range): List[Int] = 
        xs.zipWithIndex.filter { case (_, idx) => range.contains(idx) }.map(_._1)
    

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
xs.grouped(3).foreach(println)
xs.sliding(3-1,3).toList
xs.sliding(3-1,3).flatten.toList
xs.drop_every_nth(3)
xs.drop_every_nth_2(3)
xs.drop_every_nth_3(3)
xs.drop_every_nth_4(3)
xs.drop_every_nth_5(3)
xs.split(3)
xs.split_2(3)
xs.drop(3).take(7)
xs.slice(3,7)
xs.rotate(3)    
xs.rotate(-3)
xs.removeAt(3 to 3)
xs.removeAt(3 to 5)
xs.removeAt_2(3 to 5)
xs.getAt_2(3 to 5)
(xs.removeAt(3 to 5) ::: xs.getAt_2(3 to 5)).sorted == xs