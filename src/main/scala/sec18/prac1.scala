package sec18
package prac
// 高级类型

// 1.
object SHOW
object THEN
object around
class Bug(var pos: Int, var dir: Boolean) {
	def move(step: Int) : Bug = {
		if (dir) {
			pos += step
		} else {
			pos -= step
		}
		this
	}
	def turn() : Bug = {
		dir = !dir
		this
	}
	def turn(obj: around.type) : this.type = {
		dir = !dir
		this
	}
	def show(): Bug = {
		if (dir) {
			println("..%d.. -->".format(pos))
		} else {
			println("..%d.. <--".format(pos))
		}
		this
	}
	def and(obj: Any): this.type = {
		if (obj == SHOW) {
			show()
		} else {
		}
		this
	}
}

// 2.

// 3.
object Title
object Author

class Document {
    private var useNextArgAs: Any = null
    private var title: String = "title"
    private var author: String = "author"

    def set(obj: Any): this.type = { useNextArgAs = obj; this }      // object var
    def to(arg: String): this.type = {
		if (useNextArgAs == Title)
			this.title = arg
		else if (useNextArgAs == Author)
			this.author = arg
		this
	}

    def setTitle(title: String) : this.type = { this.title = title; this }
    def setAuthor(author: String) = { this }

    override def toString = "<Document title=\"%s\" author=\"%s\">".format(title, author)
}

class Book extends Document {
    def addChapter(chapter: String) = { this }
}

// 4.
import scala.collection.mutable.ArrayBuffer
class Network {
    class Member(val name: String) {
		type NM = n.Member forSome { val n: Network }
        val contacts = new ArrayBuffer[Member]

		// 表示 M 是某个 val 的嵌套类型，相同类型的就可以调用，不同类型则不能
		// def process[M <: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2)

		def equals[M <: NM] (other: M): Boolean = {
			// other.isInstanceOf[Network#Member] = true
			// other.isInstanceOf[this.type] = false why?		// 存疑，如何判断类型相符？
			println("type: " + other.isInstanceOf[NM])
			true	

			//if (other.isInstanceOf[this.type])
			//	name.equals(other.name)
			//else
			//	false
		}
    }

    private val members = new ArrayBuffer[Member]

    def join(name: String) = {
        val m = new Member(name)
        members += m
        m
    }
}

// 5.

// 6.

// 7.

// 8.

// 9.
// 如何让 Meters 只能从 Dim[Meters] 继承而不允许从 Dim[Seconds] 中继承？
/*
abstract class Dim[T] (val value: Double, val name: String) {
	protected def create(v: Double): T
	def +(other: Dim[T]) = create(value + other.value)
	override def toString() = value + " " + name
}

class Seconds(v: Double) extends Dim[Seconds](v, "s") {
	override def create(v: Double) = new Seconds(v)
}

class Meters(v: Double) extends Dim[Seconds](v, "m") {
	override def create(v: Double) = new Seconds(v)
}
*/

// 用自身类型，编译器会保证类型一致
trait Dim[T] {
	this: T =>
	val value: Double
	val name: String
	var source: T = _
	def create(v: Double): T
	def +(other: Dim[T]) = {
		other.source = this				// 此时，如果 other.source 的类型与 this 不一致，会报错
		create(value + other.value)
	}

	override def toString() = value + " " + name
}

class Seconds(v: Double) extends Dim[Seconds] {
	val value = v
	val name = "s"
	override def create(v: Double) = new Seconds(v)
}

// class Meters(v: Double) extends Dim[Seconds] {	// 编译器会报错
class Meters(v: Double) extends Dim[Meters] {
	val value = v
	val name = "m"
	override def create(v: Double) = new Meters(v)
}

// 10.
// 自身类型与 trait 从某个类型继承差不多
// 构造一个示例：使用自身类型会改变初始化和重写的顺序
class Parent {
	println("build Parent...")

	def func(str: String) {
		println("call func in Parent..." + str)
	}
}

trait TChild extends Parent {
	println("build TChild...")

	override def func(str: String) {
		super.func(str)
		println("call func in TChild..." + str)
	}
}

class Impl extends TChild {
	println("build Impl...")

	override def func(str: String) {
		super.func(str)
		println("call func in Impl..." + str)
	}
}

trait TSelfChild {
	this: Parent =>
	println("build TSelfChild...")

	def func(str: String) {
		println("call func in TSelfChild..." + str)
	}
}

class Impl2 extends Parent {
	println("build Impl2...")
}

object PracTest extends App {
    println ("sec18.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
	val bug1 = new Bug(0, true)
	bug1.move(4).show().move(6).show().turn().move(5).show()

    // 2.
    println("------------------------------  practice 2 -------------------------");
	val bug2 = new Bug(0, true)
	bug2 move 4 and SHOW and THEN move 6 and SHOW turn around move 5 and SHOW

    // 3.
    println("------------------------------  practice 3 -------------------------");
	val book3 = new Book
	book3 set Title to "Scala for the Impatient" set Author to "Cay Horstmann"
	println(book3)

    // 4.
    println("------------------------------  practice 4 -------------------------");
    val chatter = new Network
    val myFace = new Network

    val fred = chatter.join("Fred")     // type is chatter.Member
    val fred2 = chatter.join("Fred")	// type is chatter.Member
    val fred3 = myFace.join("Fred")		// type is myFace.Member

    println(fred)
    println(fred2)
    println(fred3)
    println(fred.equals(fred2))
    println(fred.equals(fred3))

    // 5.
    println("------------------------------  practice 5 -------------------------");
	// 这种情况下，m1 和 m2 不需要是相同的类型，只要是一种 NetworkMember 即可

    // 6.
    println("------------------------------  practice 6 -------------------------");
	import scala.util.Either
	import scala.math._
	type or[A, B] = Either[A, B]	// String or Int means Either[String, Int]
	// Right 表示找到 it，返回对应 index；Left 表示未找到 it，返回最靠近的 index
	def lookup(arr: Array[Int], it: Int): Int or Int = {
		// 效率高的做法应该是遍历一遍，找到最靠近的值，找到就返回下标
		// 此处试验一些高阶函数用法
		def diff(a1: Int)(a2: Int): Int = abs(a1 - a2)
		val diff_it = diff(it)_
		val aabs = arr.map(diff_it(_)).zipWithIndex

		def comp(a1: Tuple2[Int, Int], a2: Tuple2[Int, Int]): (Int, Int) = {
			if (a1._1 < a2._1) a1 else a2
		}
		val min = aabs.reduceLeft(comp(_, _))

		if (min._1 == 0) {
			Right(min._2)
		} else {
			Left(min._2)
		}
	}

	println(lookup(Array(1,3,5,7,9), 5))
	println(lookup(Array(3,4,7,9), 5))
	println(lookup(Array(3,4,7,9), 6))

    // 7.
    println("------------------------------  practice 7 -------------------------");
	def closeIfFail[T](processor: T {def close(): Unit}, func: (T) => Unit) {
		try {
			func(processor)
		} catch {
			case ex: Exception => { ex.printStackTrace(); processor.close() }
		}
	}

    // 8.
    println("------------------------------  practice 8 -------------------------");
	def printValues(f: AnyRef { def apply(x: Int): Int }, from: Int, to: Int) {
		for (i <- from to to) {
			print(f(i))
			print(", ")
		}
		println
	}

	printValues((x: Int) => x * x, 3, 6)						// 9 16 25 36
	printValues(Array(1, 1, 2, 3, 5, 8, 13, 21, 34, 55), 3, 6)	// 3 5 8 13

    // 9.
    println("------------------------------  practice 9 -------------------------");
	val second = new Seconds(1)
	val meter = new Meters(2)

	println(second)
	println(meter)
	//println(second + meter)	// compile error

    // 10.
    println("------------------------------  practice 10 -------------------------");
	val impl = new Impl
	impl.func("impl")

	println("---")
	// 这种情况下先初始化 Impl2 然后才是 TSelfChild
	// 当 trait 中定义了 func 的时候
	// func 无法放在 Impl2 中定义，只能放在实例中重写，否则会有继承问题

	val impl2 = new Impl2 with TSelfChild {
		override def func(str: String) {
			super.func(str)
			println("call func in Impl2..." + str)
		}
	}
	impl2.func("impl2")
}
