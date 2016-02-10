package sec10
// 特质

// 1.
trait RectangleLike {
    def getX(): Double
    def getY(): Double
    def getWidth(): Double
    def getHeight(): Double

    def setFrame(x: Double, y: Double, w: Double, h: Double): Unit

    // new rectangle: top-left(x - h, y + v), width = width + 2h, height = height + 2v
    def grow(h: Int, v: Int) {
        val x = getX - h
        val y = getY + v
        val width = getWidth + 2 * h
        val height = getHeight + 2  * v 
        setFrame(x, y, width, height)
    }

    // new rectangle: top-left(ori.x + x, ori.y + y)
    def translate(x: Int, y: Int) {
        val nx = getX + x
        val ny = getY + y
        val width = getWidth
        val height = getHeight
        setFrame(nx, ny, width, height)
    }
}

// 2.
import java.awt.Point
import scala.math.Ordered
class OrderedPoint(x: Int, y: Int) extends Point(x, y) with Ordered[OrderedPoint] {
    def compare(that: OrderedPoint): Int = {
        val res: Int =
            if (x < that.x) -1 
            else if (x == that.x && y < that.y) -1
            else if (x == that.x && y == that.y) 0
            else 1
        res
    }
    override def toString = "OrderedPoint: (%d, %d)".format(x, y)
}

// 3.

// 4.

// 5.
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
trait PropertyChangeSupport {
    val source: AnyRef
    var listener_map = new HashMap[String, ArrayBuffer[PropertyChangeListener]]
    //def getPropertyChangeListener(): HashMap [String, ArrayBuffer[PropertyChangeListener]] { listener_map }

    def addPropertyChangeListener(propertyName: String, listener: PropertyChangeListener) {
        listener_map.get(propertyName) match {
            case Some(ab) => ab += listener
            case None => listener_map(propertyName) = ArrayBuffer(listener)
        }
    }
    def removePropertyChangeListener(propertyName: String, listener: PropertyChangeListener) {
        listener_map.get(propertyName) match {
            case Some(ab) => ab -= listener
            case None => 
        }
    }

    def firePropertyChange(propertyName: String, oldValue: Int, newValue: Int) {
        listener_map.get(propertyName) match {
            case Some(ab) => {
                val event = new PropertyChangeEvent(source, propertyName, oldValue, newValue)
                for (elem <- ab) elem.propertyChange(event)
            }
            case None => 
        }
    }

    def hasListeners(propertyName: String): Boolean = {
        listener_map.get(propertyName) match {
            case Some(ab) => true
            case None => false
        }
    }
}
class LabeledPoint(val label: String, x: Int, y: Int) extends Point(x, y) with PropertyChangeSupport {
    val source = this

    // class can override method in super trait
    // override def hasListeners(propertyName: String): Boolean = { false }

    def setX(x: Int): LabeledPoint = {
        val oldx = this.x
        val res = new LabeledPoint(label, x, this.y)
        firePropertyChange("x", oldx, x)
        res.listener_map = this.listener_map
        res
    }
    def setY(y: Int): LabeledPoint = {
        val oldy = this.y
        val res = new LabeledPoint(label, this.x, y)
        firePropertyChange("y", oldy, y)
        res.listener_map = this.listener_map
        res
    }
}

// 6.

// 7.
import scala.xml._
trait HtmlStyle {
    def wrap(): NodeSeq
}

trait PlainTextStyle extends HtmlStyle {
    this: {def toString(): String} =>
    override def wrap(): NodeSeq = {
        val items = new NodeBuffer
        items += <text>{toString}</text>
        val nodes: NodeSeq = items
        nodes
    }
}

trait CenterStyle extends HtmlStyle {
    override abstract def wrap(): NodeSeq = {
        val items = new NodeBuffer
        items += <center>{super.wrap()}</center>
        val nodes: NodeSeq = items
        nodes
    }
}

trait BoldStyle extends HtmlStyle {
    override abstract def wrap(): NodeSeq = {
        val items = new NodeBuffer
        items += <bold>{super.wrap()}</bold>
        val nodes: NodeSeq = items
        nodes
    }
}

// 8.
/*
关于自身类型的名称写法
All three forms are valid, and have the effect that B is assumed as the type of this in class A. The variants

trait A { self: B => ... }
trait A { foo: B => ... }
introduce self (respectively, foo) as an alias for this in trait A. This is useful for accessing the this reference from an inner class. I.e. you could then use self instead of A.this when accessing the this reference of the trait A from a class nested within it. The third variant,

trait A { this: B => ... }
does not introduce an alias for this; it just sets the self type.
*/
import java.io.File
import java.io.FileInputStream
import java.io.InputStream
trait BufferedInputStream extends Logged {
    this: InputStream =>
    /*
    abstract override def read(): Int = {
        val res = super.read()      // because super.read is abstract, so this override 
                                    // should be abstract
        println("after read...")
        res
    }
    */

    var byarray: Array[Byte] = new Array[Byte](4096)
    var index: Int = 0
    var length: Int = 0

    // if byarray is not empty and index is not end, return the next byte
    // else read buffer from input stream, reset index
    // return -1 if reach EOF
    override def read(): Int = {
        var res: Int = -1
        if (!byarray.isEmpty) {
            if (index >= length) {
                index = 0
                length = read(byarray)
                log("read more to buffer..." + length)
                if (length == -1) {
                    res = -1
                } else {
                    res = byarray(index)
                    index += 1
                }
            } else {
                res = byarray(index)
                index += 1
            }
        } else {
            length = read(byarray)
            log("first read to buffer..." + length)
            if (length == -1) {
                res = -1
            } else {
                res = byarray(index)
                index += 1
            }
        }
        res
    }
}

// 9.
trait Logged {
    def log(msg: String): Unit
}

trait ConsoleLogger extends Logged {
    override def log(msg: String) { println(msg) }
}

trait TimestampLogger extends Logged {
    override abstract def log(msg: String) {
        super.log(new java.util.Date() + " " + msg)
    }
}

trait ShortLogger extends Logged {
    val maxLength = 15
    override abstract def log(msg: String) {
        super.log (
            if (msg.length <= maxLength) msg else msg.substring(0, maxLength - 3) + "..."
        )
    }
}

// 10.
import scala.collection.Iterable
import scala.collection.Iterator
trait IterableInputStream extends InputStream with Iterable[Byte] with Logged {

    var theNext: Byte = -1
    override def iterator: Iterator[Byte] = {
        val a: Iterator[Byte] = new Iterator[Byte] {
            override def hasNext: Boolean = {
                theNext = read().toByte
                if (theNext == -1) false else true
            }
            override def next: Byte = { log(theNext.toChar.toString); theNext }
        }
        a
    }
}

object PracTest extends App {
    println ("sec10.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    def printRectangleLike(rec: RectangleLike) {
        val str = "(x=%5.2f, y=%5.2f, w=%5.2f, h=%5.2f)".format(rec.getX, rec.getY, rec.getWidth, rec.getHeight)
        println(str)
    }
    val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
    printRectangleLike(egg)
    egg.translate(10, -10)
    printRectangleLike(egg)
    egg.grow(10, 20)
    printRectangleLike(egg)

    // 2.
    println("------------------------------  practice 2 -------------------------");
    import scala.collection.mutable.ArrayBuffer
    val parray = ArrayBuffer(new OrderedPoint(3, 2), new OrderedPoint(3, 3), new OrderedPoint(1, 5), new OrderedPoint(3, 1))
    println (parray.mkString(" | "))
    println (parray.sorted.mkString(" | "))
    println (parray.sortWith(_ > _).mkString(" | "))

    // 3.
    println("------------------------------  practice 3 -------------------------");
    // trait scala.collection.BitSet extends SortedSet[Int] with BitSetLike[BitSet]
    // trait scala.collection.BitSetLike[+This <: BitSetLike[This] with SortedSet[Int]] extends SortedSetLike[Int, This]
    // trait scala.collection.SortedSetLike[A, +This <: SortedSet[A] with SortedSetLike[A, This]] extends Sorted[A, This] with SetLike[A, This]
    // trait scala.collection.SetLike[A, +This <: SetLike[A, This] with Set[A]] extends IterableLike[A, This] with GenSetLike[A, This] with Subtraactable[A, This] with Parallelizable[A, ParSet[A]]
    // trait scala.collection.SortedSet[A] extends Set[A] with SortedSetLike[A, SortedSet[A]]
    // trait scala.collection.Set[A] extends (A) => Boolean with Iterable[A] with GenSet[A] with GenericSetTemplate[A, Set] with SetLike[A, Set[A]]
    // trait scala.collection.GenSet[A] extends GenSetLike[A, GenSet[A]] with GenIterable[A] with GenericSetTemplate[A, GenSet]
    // trait scala.collection.generic.Sorted[K, +This <: Sorted[K, This]] extends AnyRef

    // abstract class scala.collection.immutable.BitSet extends AbstractSet[Int] with SortedSet[Int] with collection.BitSet with BitSetLike[BitSet] with Serializable
    // class scala.collection.mutable.BitSet extends AbstractSet[Int] with SortedSet[Int] with collection.BitSet with BitSetLike[BitSet] with SetLike[Int, BitSet] with Serializable

    // 4.
    println("------------------------------  practice 4 -------------------------");

    // 5.
    println("------------------------------  practice 5 -------------------------");
    var point: LabeledPoint = new LabeledPoint("danny", 5, 10)
    println(point)
    point.addPropertyChangeListener("x", new PropertyChangeListener(){
            def propertyChange(evt: PropertyChangeEvent) {
                val pt = evt.getSource.asInstanceOf[LabeledPoint]
                println("change Point<%s>.X from %s to %s".format(pt.label, evt.getOldValue, evt.getNewValue))
            }
        })
    point.addPropertyChangeListener("y", new PropertyChangeListener(){
            def propertyChange(evt: PropertyChangeEvent) {
                val pt = evt.getSource.asInstanceOf[LabeledPoint]
                println("change Point<%s>.Y from %s to %s".format(pt.label, evt.getOldValue, evt.getNewValue))
            }
        })
    point = point.setX(8)
    println(point)
    point = point.setY(20)
    println(point)

    // 6.
    println("------------------------------  practice 6 -------------------------");
    // in java:
    //  JPanel should extends from JComponent and JContainer
    //  so JComponent should extends from JComponent
    // in scala:
    //  class JPanel extends JContainer with JComponent

    // 7.
    println("------------------------------  practice 7 -------------------------");
    val xmlnode = new Text("Danny") with PlainTextStyle with CenterStyle with BoldStyle
    println(xmlnode.wrap())     // bold center text
    val xmlnode2 = new Text("Danny") with PlainTextStyle with BoldStyle with CenterStyle
    println(xmlnode2.wrap())    // center bold text

    // 8.
    println("------------------------------  practice 8, and 9 -------------------------");
    val is = new FileInputStream(new File("./s/sec9/prac1.scala")) with BufferedInputStream with ConsoleLogger with TimestampLogger with ShortLogger
    val i1 = is.read()
    val i2 = is.read()
    println (i1.toChar)
    println (i2.toChar)
    is.close()

    // 9.
    println("------------------------------  practice 9 -------------------------");
    val is2 = new FileInputStream(new File("./s/sec9/prac1.scala")) with BufferedInputStream with ConsoleLogger with ShortLogger with TimestampLogger
    val i3 = is2.read()
    val i4 = is2.read()
    println (i1.toChar)
    println (i2.toChar)
    is2.close()

    // 10.
    println("------------------------------  practice 10 -------------------------");
    val is3 = new FileInputStream(new File("./s/sec9/prac1.scala")) with IterableInputStream with ConsoleLogger with TimestampLogger with ShortLogger {
        override def log(msg: String) { print(msg) }    // will override all of the logger trait
    }
    val iter: Iterator[Byte] = is3.iterator
    while (iter.hasNext) {
        iter.next
    }
    is3.close
    println()
}
