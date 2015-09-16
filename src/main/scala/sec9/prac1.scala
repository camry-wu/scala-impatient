package sec9

// 1.
import scala.io.Source
import scala.collection.mutable.ArrayStack
class InvertFile(val filename: String) {
    def invert() {
        val source = Source.fromFile(filename)
        val store = new ArrayStack[String]()
        val lineIterator = source.getLines
        for (line <- lineIterator) {
            store += line
        }
        source.close

        for (line <- store) {
            println(line)
        }
    }
}

object InvertFile {
    def apply(filename: String): InvertFile = {
        val res = new InvertFile(filename)
        res
    }
}

// 2.

// 3.

// 4.

// 5.

// 6.

// 7.

// 8.

// 9.

// 10.

object PracTest extends App {
    println ("sec9.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    val test1 = InvertFile("./s/sec1/prac1.scala");
    test1.invert

    // 2.
    import java.io.PrintWriter
    println("------------------------------  practice 2 -------------------------");
    def tab2blank(filename: String, outfile: String) {
        val out = new PrintWriter(outfile)
        val source = Source.fromFile(filename)
        val lineIterator = source.getLines
        for (line <- lineIterator) {
            def newline = line.replace("\t", "    ");
            out.println(newline)
        }
        source.close
        out.close
    }
    tab2blank("./s/sec9/test2.txt", "./s/sec9/test2.txt.blank");
    println("ok.")

    // 3.
    println("------------------------------  practice 3 -------------------------");
    def findMoreThan12Word(filename: String) {
        val source = Source.fromFile(filename)
        source.mkString.split("[\\s\\._()\"{}]+").filter(_.length > 12).foreach(println(_))
        source.close
    }
    findMoreThan12Word("./s/sec1/prac1.scala")

    // 4.
    println("------------------------------  practice 4 -------------------------");
    def summary(filename: String) {
        val source = Source.fromFile(filename)
        val numberPattern = """[0-9\.]+""".r

        // val numbers = for (num <- numberPattern.findAllIn(source.mkString)) yeild num.toDouble
        // more than 1 times to travel iterator should reset iterator
        // but array needn't
        val numbers = numberPattern.findAllIn(source.mkString).toArray.map(_.toDouble)
        val length = numbers.length
        val sum = numbers.sum
        printf("sum:\t%f\n", sum)
        printf("avg:\t%f\n", sum / length)
        printf("max:\t%f\n", numbers.max)
        printf("min:\t%f\n", numbers.min)
        source.close
    }
    summary("./s/sec9/test4.txt")

    // 5.
    println("------------------------------  practice 5 -------------------------");
    def prac5(outfile: String) {
        val out = new PrintWriter(outfile)
        val orilist = 0 to 20
        val pow2 = orilist.map(scala.math.pow(2, _))
        val countdown = pow2.map(1/_)
        val pair = pow2.zip(countdown)

        for (numpair <- pair) {
            out.println("%10.0f\t\t%-1f".format(numpair._1, numpair._2))
        }
        out.close
    }
    prac5("./s/sec9/test5.txt")
    println("ok.")

    // 6.
    println("------------------------------  practice 6 -------------------------");

    // 7.
    println("------------------------------  practice 7 -------------------------");
    def prac7(filename: String) {
    }

    // 8.
    println("------------------------------  practice 8 -------------------------");

    // 9.
    println("------------------------------  practice 9 -------------------------");

    // 10.
    println("------------------------------  practice 10 -------------------------");
}
