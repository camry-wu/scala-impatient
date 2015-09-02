package sec4

class Prac1 {
}

object Prac1 extends App {
    println ("sec4.Prac1")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    val dev = Map("Cap" -> 89, "Shirt" -> 138, "Breeches" -> 98, "shoes" -> 238)

    val discountDev = for ((k, v) <- dev) yield (k, v * 0.9)

    println(dev.toString())
    println(discountDev.toString())

    // 2.
    println("------------------------------  practice 2 -------------------------");
    import scala.io.Source
    import scala.collection.mutable._
    def wordCount(in: Array[String]) : HashMap[String, Int] = {
        val res: HashMap[String, Int] = HashMap[String, Int]()

        for (elem <- in) {
            if (res.contains(elem)) {
                val org = res(elem)
                res(elem) = org + 1
            } else {
                res(elem) = 1
            }
        }

        res
    }

    val source = Source.fromFile("/tmp/A.java", "UTF-8")
    val in = source.mkString.split("""[\s;$/:~\[\]\(\)\{\}-]+""")  // =, *, @ cannot set?

    val wc = wordCount(in)
    println(wc.mkString("\n"))

    // 3.
    println("------------------------------  practice 3 -------------------------");
    import scala.collection.immutable.{Map => IMap}
    def wordCount3(in: Array[String]) : IMap[String, Int] = {
        var res: IMap[String, Int] = IMap[String, Int]()

        for (elem <- in) {
            if (res.contains(elem)) {
                val ct: Int = res(elem) + 1
                res = res + (elem -> ct)
            } else {
                res = res + (elem -> 1)
            }
        }

        res
    }

    val wc3 = wordCount3(in)
    println(wc3.mkString("\n"))

    // 4.
    println("------------------------------  practice 4 -------------------------");
    import scala.collection.immutable.SortedMap
    def wordCount4(in: Array[String]) : IMap[String, Int] = {
        var res: IMap[String, Int] = SortedMap[String, Int]()

        for (elem <- in) {
            if (res.contains(elem)) {
                val ct: Int = res(elem) + 1
                res = res + (elem -> ct)
            } else {
                res = res + (elem -> 1)
            }
        }

        res
    }

    val wc4 = wordCount4(in)
    println(wc4.mkString("\n"))

    // 5.
    println("------------------------------  practice 5 -------------------------");
    import java.util.TreeMap
    import scala.collection.JavaConversions.mapAsScalaMap
    def wordCount5(in: Array[String]) : Map[String, Int] = {
        var res: Map[String, Int] = new TreeMap[String, Int]

        for (elem <- in) {
            if (res.contains(elem)) {
                val ct: Int = res(elem) + 1
                res(elem) = ct
            } else {
                res(elem) = 1
            }
        }

        res
    }

    val wc5 = wordCount5(in)
    println(wc5.mkString("\n"))

    // 6.
    println("------------------------------  practice 6 -------------------------");

    // 7.
    println("------------------------------  practice 7 -------------------------");
    import java.util.Properties

    // 8.
    println("------------------------------  practice 8 -------------------------");

    // 9.
    println("------------------------------  practice 9 -------------------------");

    // 10.
    println("------------------------------  practice 10 -------------------------");
}
