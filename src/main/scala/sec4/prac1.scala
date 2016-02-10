package sec4
// 映射和元组

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

    // create file
    val filename = "./LICENSE"
    val file = new java.io.File(filename)
    if (!file.exists()) {
        val out = new java.io.PrintWriter(file)
        for (i <- 1 to 100) {
            out.println(i)
        }
        out.close()
    }

    val source = Source.fromFile(filename, "UTF-8")
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
    import java.util.Calendar
    val lm = new LinkedHashMap[String, Int]
    lm("Monday") = Calendar.MONDAY
    lm("Tuesday") = Calendar.TUESDAY
    lm("Wednesday") = Calendar.WEDNESDAY
    lm("Thursday") = Calendar.THURSDAY
    lm("Friday") = Calendar.FRIDAY
    lm("Saturday") = Calendar.SATURDAY
    lm("Sunday") = Calendar.SUNDAY
    println(lm.mkString("\n"))

    // 7.
    println("------------------------------  practice 7 -------------------------");
    import java.util.Properties
    import scala.collection.JavaConversions.propertiesAsScalaMap
    val props: scala.collection.Map[String, String] = System.getProperties()
    val key_length: Int = props.keySet.map(_.length).reduceLeft(math.max(_, _)) + 1

    for((k, v) <- props) {
        print(k)
        print(" " * (key_length - k.length))
        print("| ")
        println(v)
    }

    // 8.
    println("------------------------------  practice 8 -------------------------");
    def minmax(values: Array[Int]): Tuple2[Int, Int] = {
        var min: Int = Integer.MAX_VALUE
        var max: Int = Integer.MIN_VALUE

        for (elem <- values) {
            min = math.min(min, elem)
            max = math.max(max, elem)
        }
        
        (min, max)
    }

    println(minmax(Array(2, 3, 4, 1, 5, 9, 6, 7, 8)))

    // 9.
    println("------------------------------  practice 9 -------------------------");
    def lteqgt(values: Array[Int], v: Int): Tuple3[Int, Int, Int] = {
        var lt: Int = 0
        var eq: Int = 0
        var gt: Int = 0

        for (elem <- values) {
            (elem compare v) match {
                case 1 => gt += 1
                case 0 => eq += 1
                case -1 => lt += 1
            }
        }
        
        (lt, eq, gt)
    }

    println(lteqgt(Array(2, 3, 4, 1, 5, 9, 6, 7, 8), 5))

    // 10.  Vector((H, W), (e, o), (l, r), (l, l), (o, d))
    println("------------------------------  practice 10 -------------------------");
    println("Hello".zip("World"))
}
