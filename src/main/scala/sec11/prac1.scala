package sec11
// 操作符

// 1.

// 2.

// 3.
import scala.math._
class Fraction(n: Int, d: Int) {
    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d);
    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d);

    override def toString = num + "/" + den
    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
    def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)

    def +(other: Fraction): Fraction = {
        val num = this.num * other.den + this.den * other.num
        val den = this.den * other.den
        new Fraction(num, den)
    }

    def -(other: Fraction): Fraction = {
        val num = this.num * other.den - this.den * other.num
        val den = this.den * other.den
        new Fraction(num, den)
    }

    def *(other: Fraction): Fraction = {
        val num = this.num * other.num
        val den = this.den * other.den
        new Fraction(num, den)
    }

    def /(other: Fraction): Fraction = {
        val num = this.num * other.den
        val den = this.den * other.num
        new Fraction(num, den)
    }
}

// 4.
class Money(val name: String, n: Int, d: Int) {
    val number = if (d < 100) n else n + d / 100
    val decimal = if (d < 100) d else d % 100
    override def toString = "%s %d.%d".format(name, number, decimal)

    def +(other: Money): Money = {
        if (name.equals(other.name)) {
            val num = this.number + other.number
            val dec = this.decimal + other.decimal
            new Money(name, num, dec)
        } else {
            Console.err.println("Sorry, cannot add different money!")
            this
        }
    }

    def -(other: Money): Money = {
        if (name.equals(other.name)) {
            if (this >= other) {
                val num = if (this.decimal < other.decimal) this.number - 1 - other.number else this.number - other.number
                val dec = if (this.decimal < other.decimal) this.decimal + 100 - other.decimal else this.decimal - other.decimal
                new Money(name, num, dec)
            } else {
                Console.err.println("Sorry, cannot minus bigger money!")
                this
            }
        } else {
            Console.err.println("Sorry, cannot minus different money!")
            this
        }
    }

    def ==(other: Money): Boolean = {
        this.number == other.number && this.decimal == other.decimal
    }

    def >= (other: Money): Boolean = {
        (this.number > other.number) || (this.number == other.number && this.decimal >= other.decimal)
    }

    def < (other: Money): Boolean = {
        (this.number < other.number) || (this.number == other.number && this.decimal < other.decimal)
    }
}

class Doller(n: Int, d: Int) extends Money("$", n, d) {
}

// 5.
import scala.xml._
class Table {
    private var table = <table></table>
    private var rowNum: Int = 0
    private var maxColNum: Int = 0

    private var currentTR: Elem = null
    private var curColNum: Int = 0

    def | (cellText: String): Table = {
        if (table.child.isEmpty) {
            rowNum = 1
            maxColNum = 1
            curColNum = 1

            currentTR = <tr><td>{cellText}</td></tr>
            table = table.copy(child = currentTR)
            this
        } else {
            if (curColNum < maxColNum) {
                // replace curColNum <td></td> to <td>{cellText}</td>
                val newchild = currentTR.child.take(curColNum) ++ <td>{cellText}</td> ++ currentTR.child.takeRight(maxColNum - curColNum - 1)

                curColNum += 1
                currentTR = currentTR.copy(child = newchild)
                table = table.copy(child = table.child.dropRight(1) ++ currentTR)
            } else {
                curColNum += 1
                maxColNum = curColNum

                // need adjust all of the previous rows
                currentTR = currentTR.copy(child = currentTR.child ++ <td>{cellText}</td>)
                val previous: Seq[Node] = for (tr <- table.child.dropRight(1)) yield {
                    val newchild: NodeSeq = tr.child ++ <td></td>
                    val newnode: Node = <tr>{newchild}</tr>
                    newnode
                }
                table = table.copy(child = previous ++ currentTR)
            }
            this
        }
    }

    def || (cellText: String): Table = {
        curColNum = 1
        rowNum += 1
        if (maxColNum <= 0) {
            maxColNum = 1
        }
        val loop = maxColNum - 1
        val tail = for (i <- 0 until loop) yield <td></td>
        currentTR = <tr><td>{cellText}</td>{tail}</tr>
        table = table.copy(child = table.child ++ currentTR)
        this
    }

    override def toString = table.toString
}

object Table {
    def apply(): Table = { new Table() }
}

// 6.
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder
class ASCIIArt {
    val img = new ArrayBuffer[String]

    def += (line: String): this.type = {
        img += line
        this
    }

    def ++= (lines: ArrayBuffer[String]): this.type = {
        for (line <- lines) {
            img += line
        }
        this
    }

    override def toString = {
        val sb = new StringBuilder 
        for (elem <- img) {
            sb ++= elem
            sb += '\n'
        }
        sb.toString
    }

    def |+ (other: ASCIIArt): ASCIIArt = {
        val result = new ASCIIArt
        result ++= img
        result ++= other.img
        result
    }

    def -+ (other: ASCIIArt): ASCIIArt = {
        val maxline = scala.math.max(img.length, other.img.length)
        val maxval = img.map(_.length).max
        val blank = (for (i <- 1 to maxval) yield ' ').mkString

        val result = new ASCIIArt
        for (i <- 0 to maxline - 1) {
            val left_idx = (img.length - maxline) / 2 + i
            val left = if (left_idx < 0 || left_idx >= img.length) blank else img(left_idx)

            val right_idx = (other.img.length - maxline) / 2 + i
            val right = if (right_idx < 0 || right_idx >= other.img.length) "" else other.img(right_idx)
            
            result += (left + " " + right)
        }
        result
    }
}

// 7.
class BigSequence(var bitseq: Long) {
    def apply(idx: Int): Int = {
        assert(idx >= 0 && idx <= 63)

        val mask: Long = 1L << idx
        if ((mask & bitseq) == 0) 0 else 1
    }

    // println(((1L << 23) ^ -1).toBinaryString)
    def update(idx: Int, value: Int): Unit = {
        assert(idx >= 0 && idx <= 63)
        assert(value == 0 || value == 1)

        if (value == 1) {
            val mask: Long = 1L << idx
            bitseq = bitseq | mask
        } else {
            val mask: Long = (1L << idx) ^ -1L
            bitseq = bitseq & mask
        }
    }

    override def toString = bitseq.toBinaryString
}

// 8.
class Matrix(val value: Array[Array[Int]]) {
    override def toString = {
        val sb = new StringBuilder

        for (row <- value) {
            for (col <- row) {
                sb.append(col)
                sb.append(", ")
            }
            sb.append("\n")
        }
        sb.toString
    }

    def apply(row: Int, col: Int): Int = {
        value(row)(col)
    }

    def +(other: Matrix): Matrix = {
        val new_value = new ArrayBuffer[Array[Int]]
        for (i <- 0 to value.length - 1) {
            val left = value(i)
            val right = other.value(i)
            val row = for (x <- left.zip(right)) yield (x._1 + x._2)

            new_value += row
        }
        new Matrix(new_value.toArray)
    }

    // [ 1 1 ] * [0 2 3] = [1 3 5]
    // [ 2 0 ]   [1 1 2]   [0 4 6]
    // matrix1.col == matrix2.row
    def *(other: Matrix): Matrix = {
        val right_col_length = other.value(0).length
        val new_value = new ArrayBuffer[Array[Int]]

        for (i <- 0 to value.length - 1) {
            val left_row = value(i)

            val new_row = new ArrayBuffer[Int]
            for (j <- 0 to right_col_length - 1) {
                var sum = 0
                for (k <- 0 to left_row.length - 1) {
                    sum += left_row(k) * other.value(k)(j)
                }
                new_row += sum
            }

            new_value += new_row.toArray
        }

        new Matrix(new_value.toArray)
    }

    def *(fac: Int): Matrix = {
        val new_value = new ArrayBuffer[Array[Int]]
        for (arr <- value) {
            val row = arr.map(_ * fac)
            new_value += row
        }
        new Matrix(new_value.toArray)
    }
}

// 9.
import java.io.File
object RichFile {
    def unapply(input: File) = {
        val path = input.getParent
        val name = input.getName

        if (path == null && name == null) None
        else {
            val idx = name.lastIndexOf('.')
            val filename = if (idx != -1) name.substring(0, idx) else name
            val extname = if (idx != -1) name.substring(idx + 1) else null

            Some(path, filename, extname)
        }
    }
}

object RichFile2 {
    def unapplySeq(input: File): Option[Seq[String]] = {
        var path = input.getPath
        if (path == null || path.trim == "") None
        else {
            path = path.trim
            path = if(path.startsWith("/")) path.substring(1) else path

            if (path == "") None
            else Some(path.split("/"))
        }
    }
}

// 10.
object Name {
    def unapplySeq(input: String): Option[Seq[String]] = {
        if (input.trim == "") None else Some(input.trim.split("\\s+"))
    }
}

object PracTest extends App {
    println ("sec11.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    // 3 + 4 -> 5 : result is (7, 5)
    // 3 -> 4 + 5 : means (3, 4) + 5, error: type mismatch; 5 is int
    // 3 -> 4 + "5" will return (3,4)5

    // 2.
    println("------------------------------  practice 2 -------------------------");
    // BigInt.* is multiplication of bigints
    // BigInt.^ is bitwise exclusive-or of bigints
    // val m = BigInt(26)
    // val ** = m.pow _
    // **(2)  // will return 676

    // 3.
    println("------------------------------  practice 3 -------------------------");
    val frac1 = new Fraction(2, 4)      //      2/4 = 1/2
    val frac2 = new Fraction(15, -6)    //      15/-6 = -5 / 2
    println(frac1)
    println(frac2)
    println(frac1 + frac2)              // -4 / 2 = -2 / 1
    println(frac1 - frac2)              // 6 / 2 = 3 / 1
    println(frac1 * frac2)              // -5 / 4
    println(frac1 / frac2)              // -1 / 5

    // 4.
    println("------------------------------  practice 4 -------------------------");
    // donot support: money * money = ?; money / money = ?
    val money1 = new Doller(5, 94)
    val money2 = new Doller(100, 7)
    println(money1)
    println(money2)
    println(money1 + money2)
    println(money1 - money2)
    println(money2 - money1)
    println(money1 >= money2)
    println(money1 < money2)
    println(money2 >= money1)
    println(money2 < money1)

    // 5.
    println("------------------------------  practice 5 -------------------------");
    val table = Table() | "Java" | "Scala" || "Gosling" | "Odersky" | "Other" || "JVM" | "JVM, .NET"
    println(table)
    // result:
    // <table>
    //     <tr><td>Java</td><td>Scala</td><td></td></tr>
    //     <tr><td>Gosling</td><td>Odersky</td><td>Other</td></tr>
    //     <tr><td>JVM</td><td>JVM, .NET</td><td></td></tr>
    // </table>

    // 6.
    println("------------------------------  practice 6 -------------------------");
    val art1 = new ASCIIArt
    art1 += " /\\_/\\"
    art1 += "( ' ' )"
    art1 += "(  -  )"
    art1 += " | | | "
    art1 += "(__|__)"

    val art2 = new ASCIIArt
    art2 += "     @  "
    art2 += "   -----"
    art2 += " / Hello \\"
    art2 += "<  Scala |"
    art2 += " \\ Coder /"
    art2 += "   -----"
    art2 += "     @  "

    println(art1 |+ art2)
    println(art1 -+ art2)

    // 7.
    println("------------------------------  practice 7 -------------------------");
    val bigseq1 = new BigSequence(-1)   // 1111...111
    val bigseq2 = new BigSequence(0)    // 0000...000

    bigseq1(0) = 0
    bigseq1(63) = 0
    bigseq2(0) = 1
    bigseq2(63) = 1

    println(bigseq1)
    println(bigseq2)

    // 8.
    println("------------------------------  practice 8 -------------------------");
    val matv = Array(Array(2, 3), Array(3, 5), Array(7, 9))
    val mat = new Matrix(matv)
    println(mat)

    println(mat * 2)

    val matv2 = Array(Array(6, 4), Array(1, -3), Array(8, 2))
    val mat2 = new Matrix(matv2)
    println(mat2)

    println(mat + mat2)

    val matv3 = Array(Array(1, 2, 3), Array(4, 5, 6))
    val mat3 = new Matrix(matv3)
    println(mat3)

    println(mat * mat3)

    // 9.
    println("------------------------------  practice 9 -------------------------");
    val file = new File("/home/cay/danny/readme.txt")
    val RichFile(p, n, e) = file
    println(p)
    println(n)
    println(e)

    // 10.
    println("------------------------------  practice 10 -------------------------");
    file match {
        case RichFile2(a,b,c,d) => println(s"$a, $b, $c, $d")
        case RichFile2(a,b,c,d,e) => println(s"$a, $b, $c, $d, $e")
        case _ => println("nothing matched.")
    }

    /*
    val auth = "a b c d"
    auth match {
        case Name(a) => println(s"$a")
        case Name(a, b) => println(s"$a, $b")
        case Name(a, b, c) => println(s"$a, $b, $c")
        case Name(a, b, c, d) => println(s"$a, $b, $c, $d")
        case _ => println("nothing matched.")
    }
    */
}
