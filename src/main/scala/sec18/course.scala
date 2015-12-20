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

// 11.

// 12.

// 13.

// 14.

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
    type x[A, B] = (A, B)

    // 8.
    println("------------------------------  section 8 -------------------------");
    import javax.swing.JComponent
    def m81(a: Array[T] forSome { type T <: JComponent }) {}
    def m82(a: Array[_ <: JComponent]) {}

    // the follow is same
    def m83(a: Array[_]) {}
    def m84(a: Array[T] forSome {type T} ) {}

    // the follow is same
    // def m83(a: Array[_]) {}
    // def m84(a: Array[T] forSome {type T} ) {}

    // 9.
    println("------------------------------  section 9 -------------------------");

    // 10.
    println("------------------------------  section 10 -------------------------");

    // 11.
    println("------------------------------  section 11 -------------------------");

    // 12.
    println("------------------------------  section 12 -------------------------");

    // 13.
    println("------------------------------  section 13 -------------------------");

    // 14.
    println("------------------------------  section 14 -------------------------");
}
