package sec21
package course
// 隐式转换和隐式参数

// 1. 隐式转换
import scala.math._
class Fraction(n: Int, d: Int) {
    val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d);
    val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d);

    override def toString = num + "/" + den
    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
    def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)

    def +(other: Fraction): Fraction = {
        val num = this.num * other.den + this.den * other.num
        val den = this.den * other.den
        new Fraction(num, den)
    }

    def -(other: Fraction): Fraction = {
        val num = this.num * other.den - this.den * other.num
        val den = this.den * other.den
        new Fraction(num, den)
    }

    def *(other: Fraction): Fraction = {
        val num = this.num * other.num
        val den = this.den * other.den
        new Fraction(num, den)
    }

    def /(other: Fraction): Fraction = {
        val num = this.num * other.den
        val den = this.den * other.num
        new Fraction(num, den)
    }
}

// 隐式函数可以放到函数可定义的地方
// 如果放在包对象中，需要 import
// 放在伴生对象中，不需要 import
object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d)

    // 隐式函数可用 source2Target 这样的命名方式
    implicit def int2Fraction(n: Int) = Fraction(n, 1)
}

// 2. 利用隐式转换丰富现有类库的功能
import java.io.File
import scala.io.Source
class RichFile(val from: File) {
    def read = Source.fromFile(from.getPath).mkString

    //! 隐式函数定义在这个地方不起作用
    // implicit def file2RichFile(from: File) = new RichFile(from)
}

// 3. 引入隐式转换
// 在源或者目标类型的伴生对象中的隐式函数会被自动引入
// 在当前作用域内可以以单个标志符指代的隐式函数会被调用
package object utils {
    implicit def file2RichFile(from: File) = new RichFile(from)
}

object FractionConversions {
    // 把分数转为 double
    implicit def fraction2Double(f: Fraction) = f.num * 1.0 / f.den
}

// 4. 隐式转换规则
// 以下情况会尝试使用隐式转换
// .当表达式的类型与预期的类型不同时
// .当对象访问一个不存在的成员时
// .当对象调用某个方法，但参数声明与传入参数不匹配时

// 以下情况[不会]尝试使用隐式转换
// .如果代码能够不用隐式转换即通过编译
// .不会尝试同时执行多个转换
// .如果同一个被转换的对象存在二义性的转换，编译器会报错

// 使用 scalac -Xprint:typer xxx.scala 可以看到加入隐式转换后的源码

// 5. 隐式参数
// 对于隐式参数的缺省值，编译器会这样查找：
// .在当前作用域所有可以用单个标识符指代的满足类型要求的 val 和 def
// .与所要求类型相关联的类型的伴生对象。相关联的类型包括所要求类型本身，以及它的类型参数

// 注意：对于给定的数据类型，只能有一个隐式值。因此常用类型不用隐式参数

// 6. 利用隐式参数进行隐式转换
// 虽然 Fraction 没有 < 操作符，但可以被隐式转成 Ordered[Fraction]
// 而 Ordered[Fraction] 是带有 < 操作符的，可以被 smaller 函数调用
class OrderedFraction(val value: Fraction) extends Ordered[Fraction] {
    def compare(that: Fraction): Int = {
        val res1 = value.num * that.den;
        val res2 = that.num * value.den;

        if (res1 < res2)
            -1
        else if (res1 == res2) 
            0
        else
            1
    }
}

// 7. 上下文界定
// T:M 这样的上下文界定，要求作用域中有一个类型为 M[T] 的隐式值
class Pair[T : Ordering] (val first: T, val second: T) {
    def smaller(implicit ord: Ordering[T]) = 
        if (ord.compare(first, second) < 0) first else second

    // 引入从 Ordering 到 Ordered 的隐式转换
    def bigger = {
        import Ordered._;
        if (first > second) first else second
    }
}

// 8. 类型证明
// T =:= U      将验证 T 是否等于 U
// T <:< U      将验证 T 是否 U 的子类型
// T <%< U      将验证 T 是否可以被视图（隐式）转换为 U
//
// 这些做法需要提供一个隐式参数

// 可以在 REPL 中调用 implicitly[String <:< AnyRef] 来检查泛型的隐式对象是否存在

// 9. @implicitNotFound 注解
// 注解告诉编译器，当不能编译时给出的错误提示，例如：
// @implicitNotFound(msg = "Cannot prove that ${From} <:< ${To}.")
// sealed abstract class <:<[-From, +To] extends (From => To) with Serializable

// 10. CanBuildFrom 解读

object CourseTest extends App {
    println ("sec21.course.CourseTest")

    // 1.
    println("------------------------------  section 1 -------------------------");

    val frac1 = new Fraction(2, 4)      //      2/4 = 1/2
    val frac2 = new Fraction(15, -6)    //      15/-6 = -5 / 2
    println(frac1)
    println(frac2)
    println(frac1 + frac2)              // -4 / 2 = -2 / 1

    val result = 3 * Fraction(4, 5)
    println(result)

    // 2.
    println("------------------------------  section 2 -------------------------");
    // 引入局部化以避免不想要的转换发生
    import utils.file2RichFile
    val f2 = new File("project/build.properties")
    println(f2.read)

    // 3.
    println("------------------------------  section 3 -------------------------");
    // 此时引入了多种转换方式，会使用哪一个呢？
    import FractionConversions._
    // import FractionConversions.{fraction2Double => _, _} 若用这行替换上一行，就会排除 fraction2Double
    val result31 = Fraction(4, 5) * 3    // 此时会将 3 转为分数
    println(result31)

    val result32 = 3 * Fraction(4, 5)    // 此时会将分数转为 double
    println(result32)

    // 注意，如果感觉某个隐式转换未被应用，可以显示加上再看看编译的提示
    val result33 = fraction2Double(3) * Fraction(4, 5)

    // 4.
    println("------------------------------  section 4 -------------------------");

    // 5.
    println("------------------------------  section 5 -------------------------");

    // 6.
    println("------------------------------  section 6 -------------------------");
    // 需要后面的隐式参数说明可以将 T 转为 Ordered[T]，才能编译通过 a < b 
    def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]) = if (a < b) a else b
    println(smaller(40, 2))
    println(smaller("Hello", "World"))

    implicit def fraction2Ordered(src: Fraction): Ordered[Fraction] = {
        val result = new OrderedFraction(src)
        result
    }

    println(smaller(Fraction(1, 7), Fraction(2, 9)))

    // 7.
    println("------------------------------  section 7 -------------------------");
    val pair = new Pair(40, 2)  // Predef 作用域中有一个类型为 Ordering[Int] 的隐式值
    println(pair.smaller)

    // Predef 中有一个 def implicitly[T] (implicit e: T) = e 的函数
    // 调用 implicitly[Ordering[T]] 会找到一个 Ordering[T] 的隐式值
    // 在 Ordering object 中就有一个 implicit object Int extends IntOrdering

    println(pair.bigger)

    // 定义了下面这个隐式值后，就可以 new Pair[Point]，否则编译会报错
    import java.awt.Point
    implicit object PointOrdering extends Ordering[Point] {
        def compare (a: Point, b: Point) = -1
    }
    val pair7 = new Pair(new Point(5,5), new Point(6,6))

    // 8.
    println("------------------------------  section 8 -------------------------");
    // 通过隐式参数说明 C 是一个 Iterable[A] 的子类，并且编译器可以找到一个隐式转换，
    // 通过 ev(it) 得到一个 Iterable[A]
    def firstLast[A, C] (it: C) (implicit ev: C <:< Iterable[A]) = (it.head, it.last)
    firstLast[Int, List[Int]](List(1, 2, 3))

    // 9.
    println("------------------------------  section 9 -------------------------");
    // firstLast[String, List[Int]](List(1, 2, 3))      编译报错

    // 10.
    println("------------------------------  section 10 -------------------------");
    // 每个集合都在其伴生对象中提供了一个隐式的 CanBuildFrom 对象
}
