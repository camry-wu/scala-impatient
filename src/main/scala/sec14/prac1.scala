package sec14

// 1.
import scala.io.Source
import java.io.File
class FallsToTheNextCase(path: String) {
    def subdirs(dir: File): Iterator[File] = {
        val children = dir.listFiles.filter(_.isDirectory)
        children.toIterator ++ children.toIterator.flatMap(subdirs _)
    }

    def findCaseAndFallsThruCount(filename: String): Tuple2[Int, Int] = {
        val source = Source.fromFile(filename)
        val content = source.mkString
        val pat1 = """case[^:]+:""".r
        val pat2 = """[Ff]alls?thr""".r

        val caseCount = pat1.findAllIn(content).toList.size
        val fallsCount = pat2.findAllIn(content).toList.size
        source.close

        (caseCount, fallsCount)
    }

    def calFallsPersent(): Double = {
        var countArray = Array[Tuple2[Int, Int]]()
        for (d <- subdirs(new File(path))) {
            val files = d.listFiles.filter(!_.isDirectory).toIterator
            for(f <- files if f.getName.endsWith(".java")) {
                countArray = countArray :+ findCaseAndFallsThruCount(f.getCanonicalPath)
            }
        }
        val caseSum = countArray.map(_._1).sum
        val fallsSum = countArray.map(_._2).sum
        println("case count in java source: " + caseSum)
        println("fall count in java source: " + fallsSum)

        val result = (fallsSum * 100.0F)/ caseSum
        result
    }
}

// 2.

// 3.

// 4.
// 样例类经常被用于嵌套结构
abstract class Item
case class Article(description: String, price: Double) extends Item
case class Bundle(description: String, discount: Double, items: Item*) extends Item
case class Multiple(count: Int, item: Item) extends Item

// 5.

// 6.
// 密封类的所有子类都必须在与父类相同的文件中定义，从而保证可以检查模式语句的完整性
sealed abstract class BinaryTree
case class Leaf(value: Int) extends BinaryTree
case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

// 7.
sealed abstract class MulTree
case class MulTreeLeaf(value: Int) extends MulTree
case class MulTreeNode(children: MulTree *) extends MulTree

// 8.
sealed abstract class CalTree
case class CalTreeLeaf(value: Int) extends CalTree
case class CalTreeNode(op: Char, children: CalTree *) extends CalTree

// 9.

// 10.

object PracTest extends App {
    println ("sec14.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    val prac1 = new FallsToTheNextCase("./temp/sec14/javasrc/")
    //println(prac1.calFallsPersent)

    // 0.0574% falls to the next case condition

    // 2.
    println("------------------------------  practice 2 -------------------------");
    def swap2(tup: Tuple2[Int, Int]): Tuple2[Int, Int] = {
        tup match {
            case (x, y) => (y, x)
        }
    }

    val tup = (3, 5)
    println(tup)
    println(swap2(tup))

    // 3.
    println("------------------------------  practice 3 -------------------------");
    def swap3(arr: Array[Int]): Array[Int] = {
        arr match {
            case Array() => Array()
            case Array(x) => Array(x)
            case Array(x, y) => Array(y, x)
            case Array(x, y, a @ _*) => Array(y, x) ++ a
            case _ => Array()
        }
    }

    println(swap3(null).mkString(","))
    println(swap3(Array()).mkString(","))
    println(swap3(Array(3)).mkString(","))
    println(swap3(Array(3, 7)).mkString(","))
    println(swap3(Array(3, 7, 8)).mkString(","))
    println(swap3(Array(3, 7, 8, 9)).mkString(","))

    // 4.
    println("------------------------------  practice 4 -------------------------");
    def price(it: Item): Double = it match {
        case Article(_, p) => p
        case Bundle(_, disc, its @ _*) => its.map(price _).sum - disc
        case Multiple(c, it) => c * price(it)
    }

    println(Multiple(2, Article("camry", 5.4)))
    println(price(Multiple(2, Article("camry", 5.4))))

    // 5.
    println("------------------------------  practice 5 -------------------------");
    def leafSum(tree: List[Any]): Int = {
        var sum = 0
        for(a <- tree) {
            a match {
                case x: Int => sum += x
                case m: List[_] => sum += leafSum(m)
            }
        }
        sum
    }

    println(leafSum(List(List(3, 8), 2, List(5))))

    // 6.
    println("------------------------------  practice 6 -------------------------");
    def leafSum2(tree: BinaryTree): Int = {
        var sum = 0
        tree match {
            case l: Leaf => sum += l.value
            case n: Node => sum += leafSum2(n.left) + leafSum2(n.right)
        }
        sum
    }

    println(leafSum2(Node(Node(Leaf(3), Leaf(9)), Node(Leaf(2), Leaf(5)))))

    // 7.
    println("------------------------------  practice 7 -------------------------");
    def leafSum3(tree: MulTree): Int = {
        var sum = 0
        tree match {
            case l: MulTreeLeaf => sum += l.value
            case n: MulTreeNode => for (c <- n.children) sum += leafSum3(c)
        }
        sum
    }

    println(leafSum3(MulTreeNode(MulTreeNode(MulTreeLeaf(3), MulTreeLeaf(9)), MulTreeNode(MulTreeLeaf(2), MulTreeLeaf(5), MulTreeLeaf(8)), MulTreeLeaf(4))))

    // as List(List(3, 8), 2, List(5))
    println(leafSum3(MulTreeNode(MulTreeNode(MulTreeLeaf(3), MulTreeLeaf(8)), MulTreeLeaf(2), MulTreeNode(MulTreeLeaf(5)))))

    // TODO 8.
    println("------------------------------  practice 8 -------------------------");
    def eval(tree: CalTree): Int = {
        var sum = 0
        tree match {
            case l: CalTreeLeaf => sum += l.value
            case n: CalTreeNode => for (c <- n.children) sum += eval(c)
        }
        sum
    }

    println(eval(CalTreeNode('+', CalTreeNode('*', CalTreeLeaf(3), CalTreeLeaf(8)), CalTreeLeaf(2), CalTreeNode('-', CalTreeLeaf(5)))));

    // 9.
    println("------------------------------  practice 9 -------------------------");
    def sum(list: List[Option[Int]]): Int = {
        var sum = 0
        for (Some(elem) <- list) sum += elem
        sum
    }

    val olist = List(Some(5), None, Some(7), None, Some(2))
    println(sum(olist))

    // 10.
    println("------------------------------  practice 10 -------------------------");
    import scala.math._
    def compose(f: (Double) => Option[Double], g: (Double) => Option[Double]): (Double) => Option[Double] = {
        val res = (x: Double) => {
                val a = f(x)
                a match {
                    case None => None
                    case _ => g(x)
                }
            }
        res
    }

    def f(x: Double) = if (x >= 0) Some(sqrt(x)) else None
    def g(x: Double) = if (x != 1) Some(1 / (x - 1)) else None

    val h = compose(f, g)
    println(h(2))       // Some(1)
    println(h(1))       // None
    println(h(0))       // Some(-1) // Note: result is not None!
}
