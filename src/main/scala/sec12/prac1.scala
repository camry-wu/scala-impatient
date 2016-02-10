package sec12
// 高阶函数

// 1.
import scala.collection.mutable.ArrayBuffer

// 2.
import scala.math._

// 3.

// 4.

// 5.

// 6.

// 7.

// 8.

// 9.
class SeqMore[A](val arr: Array[A]) {
    def corresponds[B] (that: Seq[B], p: (A, B) => Boolean): Boolean = {
        val result = arr.corresponds(that)(p)
        result
    }
}

// 10.

object PracTest extends App {
    println ("sec12.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    def values(fun: (Int) => Int, low: Int, high: Int): ArrayBuffer[(Int, Int)] = {
        val result = new ArrayBuffer[(Int, Int)]

        for (i <- low to high) {
            val elem = (i, fun(i))
            result += elem
        }
        result
    }

    val v = values(x => x * x, -5, 5)
    println(v)

    // 2.
    println("------------------------------  practice 2 -------------------------");
    val v2 = v.map(x => x._1 * x._2)
    println(v2)
    val m = v2.reduceLeft(max(_, _))
    println(m)

    // 3.
    println("------------------------------  practice 3 -------------------------");
    def jieceng(m: Int): BigInt = {
        assert(m > 0);
        val arr = for (i <- 1 to m) yield BigInt(i)
        val result = arr.reduceLeft(_ * _)
        result
    }

    println(jieceng(50))

    // 4.
    println("------------------------------  practice 4 -------------------------");
    def jieceng2(m: Int): BigInt = {
        assert(m >= 0);
        val arr = for (i <- 1 to m) yield BigInt(i)
        val result = arr.foldLeft(BigInt(1))(_ * _)
        result
    }

    println(jieceng2(0))

    // 5.
    println("------------------------------  practice 5 -------------------------");
    def largest(fun: (Int) => Int, inputs: Seq[Int]): Int = {
        val arr = inputs.map(fun)
        val result = arr.max
        result
    }

    println(largest(x => 10 * x - x * x, 1 to 10))

    // 6.
    println("------------------------------  practice 6 -------------------------");
    def largestAt(fun: (Int) => Int, inputs: Seq[Int]): Int = {
        val arr = inputs.map(fun)
        val tupleSeq = inputs.zip(arr)

        val maxTuple = tupleSeq.reduceLeft((x, y) => if (x._2 > y._2) x else y);
        val result = maxTuple._1
        result
    }

    println(largestAt(x => 10 * x - x * x, 1 to 10))

    // 7.
    println("------------------------------  practice 7 -------------------------");
    def adjustToPair(op: (Int, Int) => Int): ((Int, Int)) => Int = {
        val result = (x: (Int, Int)) => op(x._1, x._2)
        result
    }

    println(adjustToPair(_ * _)((6, 7)))

    val pairs = (1 to 10) zip (11 to 20)
    val pairsSum = pairs.map(adjustToPair(_ + _)(_))
    println(pairsSum)

    // 8.
    println("------------------------------  practice 8 -------------------------");
    //class Seq[A] {
    //    def corresponds[B] (that: Seq[B])(p: (A, B) => Boolean): Boolean = {
    //    }
    //}

    val a = Array("Hello", "World", "camry-danny")
    val b = Array(5, 5, 11)
    val is = a.corresponds(b)(_.length == _)
    println(is);

    // 9.
    // need indicated the type of x and y
    println("------------------------------  practice 9 -------------------------");
    val aa = new SeqMore(a);
    val is2 = aa.corresponds(b, (x: String, y: Int) => x.length == y)
    println(is2);

    // 10.
    println("------------------------------  practice 10 -------------------------");
    // 需要换名，需要柯里化，比较自然一些
    def unless(condition: => Boolean)(block: => Unit) {
        if (!condition) block
    }

    val cond = true;
    if (cond) {
        println("cond is true!")
    }

    unless(!cond) {
        println("cond is not false!")
    }
}
