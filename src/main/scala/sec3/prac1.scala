package sec3
// 数组相关操作

class Prac1 {
}

object Prac1 extends App {
    println ("sec3.Prac1")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    def makeNArray(n: Int): Array[Int] = {
        assert(n > 0)

        import scala.math.BigInt._
        val res = new Array[Int] (n)
        for (i <- 0 until n) {
            val sign = if (probablePrime(50, scala.util.Random) % n % 2 == 0) -1 else 1
            res(i) = sign * (probablePrime(50, scala.util.Random) % n).intValue
        }
        res
    }

    val narray = makeNArray(5)
    println(narray.mkString(", "))

    // 2.
    println("------------------------------  practice 2 -------------------------");
    def swapNeighbor(ar: Array[Int]): Unit = {
        for (i <- 0 until ar.length) {
            if (i % 2 == 1) {
                val tmp = ar(i - 1)
                ar(i - 1) = ar(i)
                ar(i) = tmp
            }
        }
    }

    val narray2 = makeNArray(7)
    println(narray2.mkString(", "))
    swapNeighbor(narray2)
    println(narray2.mkString(", "))

    // 3.
    println("------------------------------  practice 3 -------------------------");
    import scala.collection.immutable._
    def swapNeighbor2(ar: Array[Int]): Array[Int] = {
        val tmp: IndexedSeq[Int] = for(i <- 0 until ar.length) yield {
            if (i % 2 == 0) {
                if (i + 1 < ar.length) ar(i + 1)
                else  ar(i)
            } else {
                ar(i - 1)
            }
        }
        //val res: Array[Int] = new Array(0) ++ tmp
        //val res: Array[Int] = tmp.toArray
        //res
        tmp.toArray
    }

    val narray3 = makeNArray(7)
    println(narray3.mkString(", "))
    println(swapNeighbor2(narray3).mkString(", "))

    // 4.
    println("------------------------------  practice 4 -------------------------");
    def positiveSort(ar: Array[Int]): Array[Int] = {
        // use yield
        /*
        val positive = for (elem <- ar if elem > 0) yield elem
        val other = for (elem <- ar if elem <= 0) yield elem
        positive ++ other
        */

        // use partition
        val spanUnit = ar.partition(_ > 0)
        spanUnit._1 ++ spanUnit._2
    }

    val narray4 = makeNArray(7)
    println(narray4.mkString(", "))
    println(positiveSort(narray4).mkString(", "))

    // 5.
    println("------------------------------  practice 5 -------------------------");
    def avg(ar: Array[Double]): Double = {
        var res: Double = ar.reduceLeft(_ + _) / ar.length
        res
    }

    val darray = Array(0.1, 2.2, 3.3, 4.5, 8.9)
    println(darray.mkString(", "))
    println(avg(darray))

    // 6.
    println("------------------------------  practice 6 -------------------------");
    def reverse(ar: Array[Int]): Array[Int] = {
        val n: Int = ar.length / 2;
        for (i <- 0 to n) {
            val swapIdx = ar.length - 1 - i;
            val tmp = ar(i)
            ar(i) = ar(swapIdx)
            ar(swapIdx) = tmp
        }
        ar
    }

    val narray5 = makeNArray(7)
    println(narray5.mkString(", "))
    println(reverse(narray5).mkString(", "))

    println("------- think with ArrayBuffer? ---------")
    import scala.collection.mutable._
    def reverse2(ar: ArrayBuffer[Int]): Unit = {
        /*for (i <- 0 until ar.length) {
            val tmp = ar.last
            ar.trimEnd
        }*/
    }

    val narray6 = ArrayBuffer(1, 2, 3, 4, 5, 6, 7)
    println(narray6.mkString(", "))
    reverse2(narray6)
    println(narray6.mkString(", "))

    // 7.
    println("------------------------------  practice 7 -------------------------");
    def rmDup(ar: Array[Int], output: ArrayBuffer[Int]): Unit = {
        if (!ar.isEmpty) { val h: Int = ar.head; rmDup(ar.tail.filter(_ != h), output += h); }
    }

    val narray7 = makeNArray(7)
    println(narray7.mkString(", "))
    val n8 = ArrayBuffer[Int]()
    rmDup(narray7, n8)
    println(n8.mkString(", "))
    println(narray7.distinct.mkString(", "))        // distinct is rmDup

    // 8.
    println("------------------------------  practice 8 -------------------------");
    def rmNegativeButFirst(ar: Array[Int]): Array[Int] = {
        val idxs = for (i <- 0 until ar.length if ar(i) < 0) yield i
        val idx = idxs.reverse.dropRight(1)

        val br = ar.toBuffer
        for (id <- idx) br.remove(id)
        br.toArray
    }

    val narray8 = makeNArray(7)
    println(narray8.mkString(", "))
    println(rmNegativeButFirst(narray8).mkString(", "))

    // compare performance with another two rmNegativeButFirst method mentioned in section 3.4

    // 9.
    println("------------------------------  practice 9 -------------------------");
    import scala.collection.JavaConversions._
    val ids: Array[String] = java.util.TimeZone.getAvailableIDs()
    val ids_us = ids.filter(_.startsWith("America/")).map(_.drop(8)).sorted
    println(ids_us.mkString(", "))

    // 10.
    println("------------------------------  practice 10 -------------------------");
    import java.awt.datatransfer._

    val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
    val nff: Buffer[String] = flavors.getNativesForFlavor(DataFlavor.imageFlavor)
    println(nff.mkString(", "))
}
