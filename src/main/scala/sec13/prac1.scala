package sec13
// 集合

// 1.
import scala.collection.Map
import scala.collection.Set
import scala.collection.immutable.List
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.{HashMap => MHashMap}
import scala.collection.mutable.SynchronizedMap

// 2.

// 3.

// 4.

// 5.

// 6.

// 7.

// 8.
import scala.collection.mutable.ArrayBuffer

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

    val lst3 = (lst :\ List[Int]())((x: Int, y: List[Int]) => y :+ x)
    println(lst3)       // result: List(5, 4, 3, 2, 1)

    // 7.
    println("------------------------------  practice 7 -------------------------");
    val prices = 1 to 5
    val quantities = 11 to 15
    val pq = prices zip quantities
    println(pq map { p => p._1 * p._2 })
    println(pq.map(Function.tupled[Int, Int, Int]((x, y) => x * y)(_)))
    println(pq.map(Function.tupled[Int, Int, Int](_ * _)(_)))
    // println(pq.map(Function.tupled(_ * _)(_)))   // compile error

    // 8.
    println("------------------------------  practice 8 -------------------------");
    def to2Array(arr: Array[Int], col: Int): Array[Array[Int]] = {
        val result = new ArrayBuffer[Array[Int]]()
        val x = arr.grouped(col)
        for (a <- x) {
            result += a
        }
        result.toArray
    }

    val arr = Array(1, 2, 3, 4, 5, 6)
    val brr = to2Array(arr, 3)
    for (a <- brr) {
        print("[")
        print(a.mkString(","))
        println("], ")
    }

    // 9.
    println("------------------------------  practice 9 -------------------------");
    // val frequencies = new MHashMap[Char, Int] with SynchronizedMap[Char, Int]
    // new Thread() {
    //     for (get c from file) {
    //         frequencies(c) = frequencies.getOrElse(c, 0) + 1
    //     }
    // }.start()
    // 
    // 注意：两个线程并发访问 getOrElse 的时候可能是得到相同的数字
    // 再继续执行增加的方法，就可能得到错误的结果
    // 如：thead a 读到 1, 应该设置为 2
    // 而  thead b 也读到 1, 也会设置为 2
    // 最后结果设置为 2, 但实际上应该是 3 了

    import scala.collection.JavaConversions.mapAsScalaConcurrentMap
    //val frequencies: scala.collection.mutable.ConcurrentMap[Char, Int] =
    //    new java.util.concurrent.ConcurrentHashMap[Char, Int]
    // 注意，并没有 mutable.ConcurrentMap class，要从什么地方找？
    
    // 即使使用 ConcurrentMap，如果按上面的做法，也是同样的情况
    // ConcurrentMap 只能保证在你访问 concurrentMap 提供的方法的时候是安全的
    // 不能保证访问两个 concurrentMap 方法（get 然后再 put）是同步的
    // 因此，ConcurrentMap 还提供了 putIfAbsent, remove, replace 等原子方法，
    // 用来将读与写操作合成原子操作
    // 正确做法如下：
    def charCount(ori: MMap[Char, Int], ch: Char): MMap[Char, Int] = {
        val res = ori
        res(ch) = res.getOrElse(ch, 0) + 1
        res
    }

    // 10.
    println("------------------------------  practice 10 -------------------------");
    // 如同第九题的情况一样，这样做不是线程安全的，最后的结果并不一定正确
    // 正确做法如下：

    def charCountAgg(map1: MMap[Char, Int], map2: MMap[Char, Int]): MMap[Char, Int] = {
        val res = map1
        for((k, v) <- map2) res(k) = res.getOrElse(k, 0) + v
        res
    }

    def agg(src: String): MMap[Char, Int] = {
        val result = src.par.aggregate(MMap[Char, Int] ())(charCount(_, _), charCountAgg(_, _))
        result
    }

    println("char count of: Hello camry and danny!")
    println(agg("Hello camry and danny!"))

    // 似乎用 aggregate 执行，是将所有元素分别进行同步运算，然后又对运算结果做合并，就像
    // hadoop 的 map reduce 一样，具体是不是这样，要进一步体会
}
