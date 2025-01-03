val inputDiskMap = """2333133121414131402"""

import scala.annotation.tailrec
case class DiskMapItem(id: Int,fileLength: Int,freeSpaceLength: Int)
type Disk = Seq[Option[Int]] // item has an id or none

sealed trait Block
object Block {
  case class Free(id: Int) extends Block
  case class File(size: Int, name: String) extends Block
}

def processBlock(maybeBlock: Option[Block]): String = maybeBlock match {
    //case Some(last@Block.Free(_)) =>
    //    s"Found a free block: $last"
    case Some(Block.Free(last)) =>
        s"Found a free block: $last"
    case Some(last @ Block.File(size, _)) =>
        s"Found a file block of size $size: $last"
    case None =>
        "No block found"
}

// Test
val freeBlock = Some(Block.Free(123))
val fileBlock = Some(Block.File(512, "example.txt"))
val noBlock = None

println(processBlock(freeBlock))  
println(processBlock(fileBlock)) 
println(processBlock(noBlock))   

extension( xs : List[DiskMapItem])
    def toDisk(): Disk =
        xs.flatMap { item => 
            List.fill(item.fileLength)(Some(item.id)) ++
            List.fill(item.freeSpaceLength)(None)
        }.toSeq
        

def strToDiskMap(input: String): List[DiskMapItem] = 
    var index = 0L
    input.grouped(2).map(p=> p.grouped(1).map(_.toInt).toList).zipWithIndex.map { 
        case (List(fileLength,freeSpaceLength),index) =>  DiskMapItem(index,fileLength,freeSpaceLength)
        case (List(fileLength),index) => DiskMapItem(index,fileLength,0) 
        case (_,index) => DiskMapItem(index,0,0)
    }.toList

def printDiskMapItem(item: DiskMapItem): Unit =
    print(s"${item.id}" * item.fileLength.toInt)
    print("." * item.freeSpaceLength.toInt)


def printDisk(disk: Disk): Unit = 
    disk.map {
        case Some(id) => print(id)
        case None => print('.')
    }

val x = (1 to 3).toSeq
val y = x.last +: x.tail.init

val z = y.last +: y.tail.init
//val a = z.last +: 
//z.tail.init
1 +: List(2)
def compact(disk: Disk): (Disk,Int) =
  @tailrec
  def compactRec(disk: Disk, compactDisk: Disk, emptyRec: Int): (Disk,Int) =
    println(s"${printDisk(disk) } <-> ${ printDisk(compactDisk) } emptyRec: ${emptyRec}")
    
    if disk.isEmpty then
        println(s"Processed ${compactDisk}")
        (compactDisk,emptyRec)
    else
        //println(s"head ${disk.head} disk.tail.init ${disk.tail}")
        disk.head match
            case None if !disk.tail.isEmpty  => compactRec(disk.last +: disk.tail.init, compactDisk,emptyRec+1) // move the last upfront
            case None => compactRec(Nil, compactDisk,emptyRec+1)
            case file@Some(_) => compactRec(disk.tail, compactDisk :+ file,emptyRec) 
            
  compactRec(disk, Vector.empty,0)


strToDiskMap("12345").map(di => s"${di.id}" * di.fileLength.toInt)
strToDiskMap("12345").map(di => s"." * di.freeSpaceLength.toInt)

val disk = strToDiskMap("12345").toDisk()
//compact(disk)
val (cdisk,emptyRec) = compact(disk)
cdisk.foreach(o => println(o.getOrElse(-1)))
println("." * emptyRec)
val xs = strToDiskMap("12345")

println("============")    
strToDiskMap("12345").foreach(printDiskMapItem)
println("\n============")
strToDiskMap("90909").foreach(println)
strToDiskMap("90909").foreach(printDiskMapItem)




