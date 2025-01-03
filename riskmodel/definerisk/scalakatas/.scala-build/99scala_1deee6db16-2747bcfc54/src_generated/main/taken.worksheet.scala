

final class taken$u002Eworksheet$_ {
def args = taken$u002Eworksheet_sc.args$
def scriptPath = """.\taken.worksheet.sc"""
/*<script>*/
import java.io.PrintWriter

val multilineText = """
CUST_ID
CUST_TIER
CUST_NAME
ORDER_AMT
10110102
Gold
Brosseau, Derrick
63508.12
10110109
Platinum
Acheson, Jeff
139824.15
10110143
Silver
Cudell, Bob
49614.00
10110211
Silver
Amico, Paul
47677.30
10110215
Platinum
Bergeron, Kim
148871.25
10110224
Silver
Madison, Shelley
40497.10
10110235
Gold
Anderson, Rick
50429.27
10110236
Silver
Tucker, Paul
42585.00
10110237
Silver
Smith, Robert
38563.98
10110393
Gold
Washington, Rochelle
73767.96
10110425
Gold
Nguyen, Trang
65522.25
10110434
Silver
Keane, Thomas
38055.40
10110436
Platinum
Catherwood, Jennifer
117107.44
10110442
Platinum
Charest, Walter
126618.60
10110458
Gold
Coutts, Sylvain
70646.32
10110497
Platinum
Zheng, Wei
191422.00
10110506
Gold
Gonzales, Roberto
79342.90
10110526
Gold
Vanelo, Susan
81978.06
10110528
Platinum
Abedini, John
136506.32
10110530
Silver
Sousa, Maria
10155.42
"""
val lines = multilineText.split("\n")
val lines_clean = lines.toList match 
    case _ :: tail => 
        tail 

val header = lines_clean.take(4)
val data = lines_clean.drop(4)
//val r = lines_clean.grouped(4)
println(header)
val d = lines_clean.grouped(4)

val writer = new PrintWriter("customer.csv")
d.foreach { l =>
    val lt = l.map(s => s.trim())
    writer.println(lt.mkString("'","','","'"))
}

//d.foreach(r => writer.println(s"${r(0)},${r(1)},${r(2)},${r(3)}"))
writer.close()

/*</script>*/ /*<generated>*//*</generated>*/
}

object taken$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new taken$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export taken$u002Eworksheet_sc.script as `taken.worksheet`

