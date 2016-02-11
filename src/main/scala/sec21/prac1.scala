package sec21
// 隐式转换和隐式参数

// 1.
object Utils {
    // 隐式类要放在 object 里头
    implicit final class CamryAssoc[A](private val self: A) extends AnyVal {
        // 内联注解可以让方法调用语句替换为被调用的方法体
        @inline def camry[B](y: B): Tuple2[A, B] = Tuple2(self, y)
    }
}

// 2.
import scala.math._
class MyRichInt(val from: Int) {
    def +% (per: Int): Int = {
        val p = if (abs(per) > 100) abs(per) % 100 else abs(per)
        val res = from + from * per / 100
        res
    }

    def ! = {
        if (from <= 0) 1 else MyRichInt.jieceng(from, from - 1)
    }
}

import scala.annotation._
object MyRichInt {
    implicit def int2MyRichInt(from: Int) = new MyRichInt(from)

    @tailrec
    def jieceng(result: BigInt, x: Int): BigInt = {
        if (x == 0) result
        else jieceng(result * x, x - 1)
    }
}

// 3.

// 4.
import java.lang.System
import scala.io.StdIn

// 5.

// 6.
import java.awt.Point
object PointOrdMethod {
    // 根据字典顺序
    implicit object PointOrderingByDict extends Ordering[Point] {
        def compare (a: Point, b: Point) = {
            println("order point base on dictionary")
            val res = a.x - b.x
            if (res != 0) res else a.y - b.y
        }
    }

    // 根据距离原点的远近程度排
    implicit object PointOrderingByDistance extends Ordering[Point] {
        def compare (a: Point, b: Point) = {
            println("order point base on distance of (0,0) ")
            val res = a.distance(0, 0) - b.distance(0, 0)
            if (res < 0) -1 else if (res == 0) 0 else 1
        }
    }

    implicit def point2Ordered(a: Point): Ordered[Point] = 
        new Ordered[Point] { def compare(that: Point): Int = PointOrderingByDistance.compare(a, that) }
}

// 7.

// 8.

// 9.

// 10.

object PracTest extends App {
    println ("sec21.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    println("Hello" -> 42)      // 实际上表示 "Hello".->(42)
    println(42.->("Hello"))

    // String 并没有 -> 方法，而是 Predef 中的隐式对象 ArrowAssoc 有这个方法
    // 应该有一个隐式调用可以将 String 转成 ArrowAssoc[String] 对象
    // 然后调用 ArrowAssoc[String].-> 方法
    // Predef.scala 源码中写到 any2ArrowAssoc[A](x: A): ArrowAssoc[A] 方法是 deprecated
    // 而 ArrowAssoc 则是 implicit 的，是否表示隐式声明的类会被自动应用到对象上？
    // 通过 CamryAssoc 实验，看来 camry() 确实自动应用到 Int 上了
    import Utils._
    println(44 camry "Danny")

    // 2.
    println("------------------------------  practice 2 -------------------------");
    // 由于 Int 没有定义 +% 方法，会被隐式转为 MyRichInt
    import MyRichInt._
    println ( 120 +% 10 )

    // 3.
    println("------------------------------  practice 3 -------------------------");
    println (5!)
    println (-1!)
    println (0!)
    println (1!)
    println (100!)

    // 4.
    println("------------------------------  practice 4 -------------------------");
    /*
    val username: String = System.getProperty("user.name")
    println(username + ", Please input your password: ")
    val pass: String = StdIn.readLine()
    if ("secret".equals(pass)) {
        Console.out.println("Welcome " + username + "!")
    } else {
        Console.err.println("Sorry, username and password unmatched!")
    }
    */

    // Read in aString askingFor "Your name" and anInt askingFor "Your age" and aDouble askingFor "Your weight"
    // println("name   = " + aString)
    // println("age    = " + anInt)
    // println("weight = " + aDouble)

    // 翻译过来就是
    // Read.in(aString).askingFor("Your name").and(anInt).askingFor("Your age").and(aDouble).askingFor("Your weight")

    // 5.
    println("------------------------------  practice 5 -------------------------");
    // 参见 sec21/course.scala 第 6 节
    // 需要提供一个隐式转换，将 Fraction 转成一个 Ordered[Fraction]

    // 6.
    println("------------------------------  practice 6 -------------------------");
    def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]) = if (a < b) a else b

    import PointOrdMethod.PointOrderingByDict           // 根据字典顺序排
    //import PointOrdMethod.PointOrderingByDistance     // 根据原点距离排
    val res6 = smaller(new Point(0,9), new Point(6,6))
    println(res6)

    // 7.
    println("------------------------------  practice 7 -------------------------");
    // import 不同的排序隐式对象进入作用域，可以切换排序方式
    // 但如果想在同一个作用域内使用不同的排序方式应该如何做？

    // 可以定义一个隐式转换函数，看来有这种函数的话会首先调用这个函数
    // 没有找到的情况下再找别的方式
    import PointOrdMethod.point2Ordered
    val res7 = smaller(new Point(0,9), new Point(6,6))

    // 也可以显示的调用一个转换函数来切换排序方式
    //val res7 = smaller(new Point(0,9), new Point(6,6))(PointOrdMethod.point2Ordered)

    println(res7)

    // 8.
    println("------------------------------  practice 8 -------------------------");
    // 在 activator 命令中输入 console 可以进入 REPL

    // 输入
    // import java.awt.Point
    // import sec21.PointOrdMethod.point2Ordered
    // implicitly[Ordering[Point]]
    // 得到 res6: Ordering[java.awt.Point] = scala.math.LowPriorityOrderingImplicits$$anon$6@463f0d46

    // 输入
    // import sec21.PointOrdMethod._
    // implicitly[Ordering[Point]]
    // 得到
    // <console>:13: error: ambiguous implicit values:
    // both object PointOrderingByDict in object PointOrdMethod of type sec21.PointOrdMethod.PointOrderingByDict.type
    // and object PointOrderingByDistance in object PointOrdMethod of type sec21.PointOrdMethod.PointOrderingByDistance.type
    // match expected type Ordering[java.awt.Point]

    // 9.
    println("------------------------------  practice 9 -------------------------");
// Predef.scala 中关于 =:= 原代码如下
 /** An instance of `A =:= B` witnesses that the types `A` and `B` are equal.
   *
   * @see `<:<` for expressing subtyping constraints
   */
/*
  @implicitNotFound(msg = "Cannot prove that ${From} =:= ${To}.")
  sealed abstract class =:=[From, To] extends (From => To) with Serializable
  private[this] final val singleton_=:= = new =:=[Any,Any] { def apply(x: Any): Any = x }
  object =:= {
     implicit def tpEquals[A]: A =:= A = singleton_=:=.asInstanceOf[A =:= A]
  }
*/
    // 这是一种类型证明，可以证明 A 和 B 是相等的
    // 在 Predef 中有一个 =:= 的类定义，并有一个私有成员实现这个类，提供了 apply 方法
    // 当需要证明 A 即是 B 时，就可以找到这个隐式转换，将 A 转成 B
    // 这样，在 A 对象上调用 B 的方法的时候，就不会报编译的错误了

    // 10.
    println("------------------------------  practice 10 -------------------------");
    
    // 而 toUpper 方法是 Char 的方法
    println("abc".map(_.toUpper))       // print: ABC

    // 而 toInt 方法是 Char 的方法
    println("abc".map(_.toInt))         // print: Vector(97, 98, 99)

    // scalac -Xprint:typer a.scala 
    // 得到如下输出：
    /*
        [[syntax trees at end of                     typer]] // a.scala
        package classes {
          object A extends AnyRef with App {
            def <init>(): classes.A.type = {
              A.super.<init>();
              ()
            };
            scala.this.Predef.println(scala.this.Predef.augmentString("Hello World!").map[Char, Any](((x$1: Char) => scala.this.Predef.charWrapper(x$1).toUpper))(scala.this.Predef.StringCanBuildFrom));
            scala.this.Predef.println(scala.this.Predef.augmentString("Hello World!").map[Int, Any](((x$2: Char) => x$2.toInt))(scala.this.Predef.fallbackStringCanBuildFrom[Int]))
          }
        }
    */

    // 解释：
    // String 没有 map 函数，但 StringOps 有，
    // Predef.scala 中有定义一个隐式转换将 String 转为 StringOps
    // @inline implicit def augmentString(x: String): StringOps = new StringOps(x)
    // StringOps.map 的定义为：def map[B](f: (A) => B): String[B]

    // 由于这个 map 方法涉及两种类型，一是展现类型 String，一个是成员类型 Char
    // 实际上，调用的 map 定义是在 FilterMonadic[+A, +Repr] 里头：
    //  abstract def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That

    // 在 TraversableLike[+A, +Repr] 里头实现的：
    /*
        def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
            def builder = { // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
              val b = bf(repr)      // 得到一个 Builder[B, That] 对象
              b.sizeHint(this)
              b
            }
            val b = builder
            for (x <- this) b += f(x)  // 对每个成员调用 f 函数得到一个结果合并到 Builder 中
            b.result                // 得到 That 对象的结果
        }
    */

    // toUpper 的情况：
    // ----------------
    // f: A => B 即为：((x$1: Char) => scala.this.Predef.charWrapper(x$1).toUpper)
    // 其中 Char 被隐式转换 charWrapper 转成了 RichChar，f 函数的结果仍然是一个 Char

    // bf 是一个隐式参数，要求 CanBuildFrom[Repr, B, That] 中的 B 是 Char
    // 在 Predef 中可以找到对应的隐式值
    //  implicit val StringCanBuildFrom: CanBuildFrom[String, Char, String] 
    //      apply 方法得到一个 StringBuilder

    // 最终的 StringBuilder.result 方法返回一个 String

    // toInt 的情况:
    // ----------------
    // f: ((x$2: Char) => x$2.toInt)，f 函数的结果是一个 Int
    // 此时，要求 CanBuildFrom[Repr, B, That] 中的 B 是个 Int, Repr 仍然是个 String
    // 于是在 Predef 中找到另外一个隐式值：
    // implicit def fallbackStringCanBuildFrom[T]: CanBuildFrom[String, T, immutable.IndexedSeq[T]]
    //      apply 方法得到一个 immutable.IndexedSeq.newBuilder[T] = Vector.newBuilder[A] = new VectorBuilder[A]
    // 而 VectorBuilder 的 result 方法返回一个 Vector
}
