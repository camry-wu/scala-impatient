package sec18
package course

// 1.
object Title

class Document {
    private var useNextArgAs: Any = null
    private var title: String = "title"

    def set(obj: Title.type): this.type = { useNextArgAs = obj; this }      // object var
    def to(arg: String) = { if (useNextArgAs == Title) this.title = arg }

    def setTitle(title: String) : this.type = { this.title = title; this }
    def setAuthor(author: String) = { this }

    override def toString = "<Document title=%s>".format(title)
}

class Book extends Document {
    def addChapter(chapter: String) = { this }
}

// 2.
import scala.collection.mutable.ArrayBuffer
class Network {
    class Member(val name: String) {
        val contacts = new ArrayBuffer[Network#Member]  // can add all Member.type of Network
    }

    private val members = new ArrayBuffer[Member]

    def join(name: String) = {
        val m = new Member(name)
        members += m
        m
    }
}

// 3.

// 4.
class Book4 {
    import scala.collection.mutable._
    type Index = HashMap[String, (Int, Int)]
}

// 5.

// 6.

// 7.

// 8.

// 9.

// 10.
trait Logged {
	def log(msg: String)
}

trait LoggedException extends Logged {
	// this: T with U with ... =>	多个自身类型
	this: Exception =>
	def log() { log(getMessage()) }
}

// this 的别名
trait Group {
	outer: Network =>
		class Member {
			// 里面可以用 outer 来指代 Group.this
		}
}

// 需要重复说明
trait ManagedException extends LoggedException {
	this: Exception =>
}

// 11.

/*
trait Logger {
	def log(msg: String)
}

trait ConsoleLogger extends Logger {
	def log(msg: String) { println("Console: " + msg) }
}

trait FileLogger extends Logger {
	def log(msg: String) { println("File: " + msg) }
}

trait Auth {
	this: Logger =>
	def login(id: String, password: String): Boolean
}

trait MockAuth extends Auth {
	this: Logger =>
	def login(id: String, password: String): Boolean = { true }
}

trait TApp {
	this: Logger with Auth =>
}

object MyApp extends TApp with FileLogger with MockAuth
*/

// 蛋糕模式实现依赖注入
// 用代码来完成依赖注入，好处在于编译器可以帮助校验类型
// 坏处在于需要编译，运行期没法灵活替换
// 不过一般情况下，运行期也不需要替换配置
trait LoggerComponent {
	trait Logger {
		def log(msg: String)
	}

	val logger: Logger
	class FileLogger(file: String) extends Logger {
		def log(msg: String) { println("File: " + msg) }
	}
}

trait AuthComponent {
	this: LoggerComponent =>
	trait Auth {
		def login(id: String, password: String): Boolean
	}

	val auth: Auth

	class MockAuth(file: String) extends Auth {
		def login(id: String, password: String): Boolean = { true }
	}
}

object AppComponents extends LoggerComponent with AuthComponent {
	val logger = new FileLogger("test.log")
	val auth = new MockAuth("users.txt")
}

// 12. 抽象类型
/*
trait Reader {
	type Contents		// 抽象类型，由具体子类来指定
	def read(fileName: String): Contents
}

import scala.io._
class StringReader extends Reader {
	type Contents = String
	def read(fileName: String) = Source.fromFile(fileName, "UTF-8").mkString
}

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
class ImageReader extends Reader {
	type Contents = BufferedImage
	def read(fileName: String) = ImageIO.read(new File(fileName))
}
*/

// 用类型参数来实现

trait Reader[C] {
	def read(fileName: String): C
}

import scala.io._
class StringReader extends Reader[String] {
	def read(fileName: String) = Source.fromFile(fileName, "UTF-8").mkString
}

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
class ImageReader extends Reader[BufferedImage] {
	def read(fileName: String) = ImageIO.read(new File(fileName))
}

// 如果类型在实例化时给出，那么用类型参数较好
// 如果类型是在子类中给出，那么用抽象类型较好

trait Listener {
	type Event <: java.util.EventObject
}

trait ActionListener extends Listener {
	type Event = java.awt.event.ActionEvent
}

// 13.

trait ListenerSupport {
	type S <: Source
	type E <: Event
	type L <: Listener

	trait Event {
		var source: S = _
	}

	trait Listener {
		def occurred(e: E): Unit
	}

	trait Source {
		this: S =>
		private val listeners = new ArrayBuffer[L]
		def add (l: L) { listeners += l }
		def remove(l: L) { listeners -= l }
		def fire(e: E) {
			e.source = this
			for (l <- listeners) l.occurred(e)
		}
	}
}

object ButtonModule extends ListenerSupport {
	type S = Button
	type E = ButtonEvent
	type L = ButtonListener 

	class ButtonEvent extends Event
	trait ButtonListener extends Listener
	class Button extends Source {
		def click() { fire(new ButtonEvent) }
	}
}

/*
 * 使用
object Main {
	import ButtonModule._
	def main(args: Array[String]) {
		val b = new Button
		b.add(new ButtonListener {
			override def occurred(e: ButtonEvent) { println(e) }
		})
		b.click()
	}
}
*/

// 14. 高等类型
import scala.collection.mutable.ArrayBuffer
trait Container[E] {
	def +=(e: E): Unit
}

trait MyIterable[E, C[X] <: Container[X]] {
	def build[F](): C[F]
	def iterator(): Iterator[E]

	def map[F](f: (E) => F): C[F] = {
		val res = build[F] ()
		val iter = iterator()
		while (iter.hasNext)
			res += f(iter.next())
		res
	}
}

class MyRange(val low: Int, val high: Int) extends MyIterable[Int, MyBuffer] {
	def iterator() = new Iterator[Int] {
		private var i = low
		def hasNext = i <= high
		def next() = { i += 1; i - 1 }
	}

	def build[F]() = new MyBuffer[F]
}

class MyBuffer[E] extends MyIterable[E, MyBuffer] with Container[E] {
	private var capacity = 10
	private var length = 0
	private var elems = new ArrayBuffer[E](capacity)

	def iterator() = new Iterator[E] {
		private var i = 0
		def hasNext = i < length
		def next() = { i += 1; elems(i - 1) }
	}

	def build[F]() = new MyBuffer[F]

	def +=(e: E) {
		if (length == capacity) {
			capacity = 2 * capacity
			val nelems = new ArrayBuffer[E](capacity)
			for (i <- 0 until length)
				nelems(i) = elems(i)
			elems = nelems
		}
		elems(length) = e
		length += 1
	}
}
/*
class MyBuffer[E: Manifest] extends MyIterable[E, MyBuffer] with Container[E] {
	private var capacity = 10
	private var length = 0
	private var elems = new Array[E](capacity)

	def iterator() = new Iterator[E] {
		private var i = 0
		def hasNext = i < length
		def next() = { i += 1; elems(i - 1) }
	}

	def build[F: Manifest]() = new MyBuffer[F]

	def +=(e: E) {
		if (length == capacity) {
			capacity = 2 * capacity
			val nelems = new Array[E](capacity)
			for (i <- 0 until length)
				nelems(i) = elems(i)
			elems = nelems
		}
		elems(length) = e
		length += 1
	}
}
*/
object CourseTest extends App {
    println ("sec18.course.CourseTest")

    // 1.
    println("------------------------------  section 1 -------------------------");
    val doc = new Document()
    doc.setTitle("title").setAuthor("author")

    val book = new Book()
    book.setTitle("title").addChapter("chapter1")

    doc.set(Title).to("camry")
    println(doc)

    // 2.
    println("------------------------------  section 2 -------------------------");
    val chatter = new Network
    val myFace = new Network

    val fred = chatter.join("Fred")     // type is chatter.Member
    val barney = myFace.join("Barney")  // type is myFace.Member

    println(fred)
    println(barney)

    fred.contacts += barney

    // 3.
    println("------------------------------  section 3 -------------------------");
    // Inner Class Path: a.b.c.type#T   => a.b.c.T
    // Path member: package, object, val, this, super, super[S], C.this, C.super, C.super[S]

    // 4.
    println("------------------------------  section 4 -------------------------");
    // type alias

    // 5.
    import java.awt.Shape
    import java.io.Serializable
    println("------------------------------  section 5 -------------------------");
    // struct type, like trait
    // indicated: abstract method, field or type
    def appendLines(target: AnyRef { var b: Int; def append(str: String): Any; type ShapeArray = ArrayBuffer[Shape with Serializable] }, lines: Iterable[String]) {
        for (l <- lines) { target.append(l); target.append("\n") }
        target.b = 5
        val img = new target.ShapeArray
        img += new java.awt.Rectangle(0, 0, 1, 1);
    }

    // 6.
    println("------------------------------  section 6 -------------------------");
    val image = new ArrayBuffer[Shape with Serializable {}]
    val rect = new java.awt.Rectangle(5, 5, 5, 5);
    image += rect
    // image += new java.awt.geom.Area(rect)   // compile failure, Area is not Serializable

    // T1 with T2 { struct type }
    import java.awt.Point
    val image2 = new ArrayBuffer[Shape with Serializable { def contains(p: Point) : Boolean}]
    image2 += rect

    // 7.
    println("------------------------------  section 7 -------------------------");
	// String Map Int		===		Map[String, Int]

    type x[A, B] = (A, B)
	// String x Int			===		(String, Int)
	// String x Int x Int	===		((String, Int), Int)

    // 8.
    println("------------------------------  section 8 -------------------------");
    import javax.swing.JComponent
    def m81(a: Array[T] forSome { type T <: JComponent }) {}
    def m82(a: Array[_ <: JComponent]) {}

    // the follow is same
    def m83(a: Array[_]) {}
    def m84(a: Array[T] forSome {type T} ) {}

    // the follow is same
    def m85(a: Map[_, _]) {}
    // def m85(a: Map[T, U] forSome {type T; type U} ) {}

    def m86(a: Map[T, U] forSome {type T; type U <: T} ) {}

	// 表示 M 是某个 val 的嵌套类型，相同类型的就可以调用，不同类型则不能
	def process[M <: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2)
	val chatter8 = new Network
	val myFace8 = new Network
	val fred8 = chatter8.join("Fred")
	val wilma8 = chatter8.join("Wilma")
	val barney8 = myFace8.join("Barney")
	process(fred8, wilma8)
	// process(fred8, barney8)	// compile error

    // 9.
    println("------------------------------  section 9 -------------------------");
	// Normal:		class C, trait C
	// Tuple:		(T1,...,Tn)
	// Function:	(T1,...,Tn) => T
	// Annotation:	T @A
	// Generic:		A[T1,...Tn]
	// Object:		val.type
	// 投影(内部类):O#I
	// 复合:		T1 with T2 with ... with Tn {...}
	// 中置:		T1 A T2
	// forSome:		T forSome { ... }

    // 10.
    println("------------------------------  section 10 -------------------------");
	// val f = new Document with LoggedException	// compile error
	val f = new Exception("error") with LoggedException {
		def log(msg: String) { println(msg) }
	}

    // 11.
    println("------------------------------  section 11 -------------------------");

    // 12.
    println("------------------------------  section 12 -------------------------");

    // 13.
    println("------------------------------  section 13 -------------------------");

    // 14.
    println("------------------------------  section 14 -------------------------");
}
