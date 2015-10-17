package sec13

// 1.
import scala.collection.Map
import scala.collection.Set
import scala.collection.immutable.List
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.{HashMap => MHashMap}

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
    println ("sec13.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    // use LinkedHashSet to save the elements insert order
    def getCharIndexMap(s: String): Map[Char, Set[Int]] = {
        val result = new MHashMap[Char, Set[Int]]() {
            override def default(key: Char): Set[Int] = {
                null
            }
        }
        for (i <- 0 to s.length - 1) {
            val ch = s(i)
            var set = result(ch)
            if (set == null) {
                set = new LinkedHashSet[Int]()
            }

            set = set + i
            result(ch) = set
        }
        result
    }

    val cim = getCharIndexMap("Mississippi")
    println(cim)

    // 2.
    println("------------------------------  practice 2 -------------------------");
    def getCharIndexMap2(s: String): Map[Char, List[Int]] = {
        val result = new MHashMap[Char, List[Int]]() {
            override def default(key: Char): List[Int] = {
                null
            }
        }
        for (i <- 0 to s.length - 1) {
            val ch = s(i)
            var list = result(ch)
            if (list == null) {
                list = List(i)
            } else {
                list = list :+ i
            }
            result(ch) = list
        }
        result
    }

    val cim2 = getCharIndexMap2("Mississippi")
    println(cim2)

    // 3.
    println("------------------------------  practice 3 -------------------------");
    def removeZeroFromList(list: List[Int]): List[Int] = {
        val idxlist = for (i <- 0 to list.length - 1 if (list(i) != 0)) yield i
        val result = for (i <- 0 to idxlist.length - 1) yield list(idxlist(i))
        result.toList
    }

    val rzfl = removeZeroFromList(List(6,2,2,6,0,9,0,1,0,0,0,1,0,3,0,1))
    println(rzfl)

    // 4.
    println("------------------------------  practice 4 -------------------------");
    def getMapInt4String(sarr: Array[String], map: Map[String, Int]): Array[Int] = {
        val result = for (str <- sarr if (map.contains(str))) yield map(str)
        result.toArray
    }

    val mi4s = getMapInt4String(Array("Tom", "Fred", "Harry"), Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5))
    println(mi4s.mkString(","))

    // 5.
    println("------------------------------  practice 5 -------------------------");
    def mkString(arr: Array[Int]): String = {
        val result = arr.map(_.toString).reduceLeft(_ + ", " + _)
        result
    }
    println(mkString(Array(1, 2, 3, 4, 3)))

    // 6.
    println("------------------------------  practice 6 -------------------------");
    val lst = List(1, 2, 3, 4, 5)
    val lst1 = (lst :\ List[Int]())(_ :: _)
    println(lst1)       // result: List(1, 2, 3, 4, 5)

    val lst2 = (List[Int]() /: lst)(_ :+ _)
    println(lst2)       // result: List(1, 2, 3, 4, 5)

    // 7.
    println("------------------------------  practice 7 -------------------------");

    // 8.
    println("------------------------------  practice 8 -------------------------");

    // 9.
    println("------------------------------  practice 9 -------------------------");

    // 10.
    println("------------------------------  practice 10 -------------------------");
}
