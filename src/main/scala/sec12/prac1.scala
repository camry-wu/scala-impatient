package sec12

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
        val arr = for (i <- 1 to m) yield BigInt(i)
        val result = arr.reduceLeft(_ * _)
        result
    }

    println(jieceng(50))

    // 4.
    println("------------------------------  practice 4 -------------------------");

    // 5.
    println("------------------------------  practice 5 -------------------------");

    // 6.
    println("------------------------------  practice 6 -------------------------");

    // 7.
    println("------------------------------  practice 7 -------------------------");

    // 8.
    println("------------------------------  practice 8 -------------------------");

    // 9.
    println("------------------------------  practice 9 -------------------------");

    // 10.
    println("------------------------------  practice 10 -------------------------");
}
