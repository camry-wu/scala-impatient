package sec15

// 1.
import org.junit.{Assert, Test, Rule}
class JTest {
    @Test
    def testA {
        println("junit test A.");
    }

    @Test(timeout = 5)
    def testB {
        println("junit test B.");
    }

    @Test(expected = classOf[java.io.IOException])
    def testC {
        println("junit test C.");
        throw new java.io.IOException()
    }

    @Test(timeout = 5, expected = classOf[java.io.IOException])
    def testD {
        println("junit test D.");
        throw new java.io.IOException()
    }
}

// 2.
// add annotation to:
// class, method, field, local var, method argument
// constructor, expression, type var,

@deprecated(message="class") class Anno @Test @deprecated(message="constructor") (var value: String) {
    @deprecated(message="member") val name = ""
    @deprecated(message="method") def a() {}

    def b(@deprecated(message="var") src: String) {
        val a: Int = (6 + 5): @deprecated(message="expression")
        @deprecated(message="local val") @deprecated(message="local val2")  val b: Int = 0
    }

    def c[@deprecated(message="var") T] (a: T) {
    }
}

// 3.

// 4.
import scala.annotation._

// 5.

// 6.
class VolatileData {
    @volatile var stopped: Boolean = false
}

class Worker1(val data: VolatileData) extends Runnable {
    def run() {
        Thread.sleep(1000)
        data.stopped = true
        println ("let data.stopped = true !")
    }
}

class Worker2(val data: VolatileData) extends Runnable {
    def run() {
        while (!data.stopped) {
            Thread.sleep(100)
        }
        println ("detected: data.stopped == true !")
    }
}

// 7.
// about @tailrec
object Util {
    @tailrec def sum(xs: Seq[Int], partial: BigInt): BigInt = {
        if (xs.isEmpty) partial else sum(xs.tail, xs.head + partial)
    }

    // set @tailrec will bring a compile error
    def sum2(xs: Seq[Int]): BigInt = {
        if (xs.isEmpty) 0 else xs.head + sum2(xs.tail)
    }
}

// cannot add @tailrec to method which can be overridden
// it is neither private nor final so can be overridden
/*
class Util2 {
    @tailrec def sum(xs: Seq[Int], partial: BigInt): BigInt = {
        if (xs.isEmpty) partial else sum(xs.tail, xs.head + partial)
    }
}
*/

// 8.

// 9.

// 10.

object PracTest extends App {
    println ("sec15.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    // 用下面的方法在 activator 中执行 JUnit
    // runMain org.junit.runner.JUnitCore sec15.JTest

    // 2.
    println("------------------------------  practice 2 -------------------------");
    // class, method, field, local var, method argument
    // constructor, expression, type var,

    // 3.
    println("------------------------------  practice 3 -------------------------");
    // compileTimeOnly: getter, setter, beanGetter, beanSetter, companionClass, companionMethod
    // deprecated: getter, setter, beanGetter, beanSetter
    // transient: field
    // volatile: field
    // BooleanBeanProperty: field

    // 4.
    println("------------------------------  practice 4 -------------------------");
    // compiler will create a desc: int var_sum(int... args)
    // so this method can be used in Java code
    @varargs def var_sum(args: Int*): Int = {
        args.sum
    }

    println(var_sum(1, 2, 3, 4, 5))

    // 5.
    println("------------------------------  practice 5 -------------------------");
    // Java code will trace the checked exception
    import scala.io.Source
    import java.io.IOException
    @throws(classOf[IOException]) def readFileContent(filename: String): String = {
        val source = Source.fromFile(filename)
        var ret = source.mkString
        ret
    }

    println(readFileContent("./temp/sec9/test2.txt.blank"))

    // 6.
    println("------------------------------  practice 6 -------------------------");
    println("thread will println after practice10 ")
    val vd: VolatileData = new VolatileData()
    new Thread(new Worker1(vd)).start
    new Thread(new Worker2(vd)).start

    // 7.
    println("------------------------------  practice 7 -------------------------");
    println(Util.sum(1 to 1000000, 0))
    // println(Util.sum2(1 to 1000000)) // Stack overflow error

    // 8.
    println("------------------------------  practice 8 -------------------------");

    def allDifferent[@specialized(Unit, Boolean, Byte, Short, Char, Int, Long, Float, Double) T] (x: T, y: T, z: T) = x != y && x != z && y != z

    println(allDifferent(3, 4, 5))

/*
    public <T extends java/lang/Object> boolean allDifferent(T, T, T);
    public boolean allDifferent$mZc$sp(boolean, boolean, boolean);
    public boolean allDifferent$mBc$sp(byte, byte, byte);
    public boolean allDifferent$mCc$sp(char, char, char);
    public boolean allDifferent$mDc$sp(double, double, double);
    public boolean allDifferent$mFc$sp(float, float, float);
    public boolean allDifferent$mIc$sp(int, int, int);
    public boolean allDifferent$mJc$sp(long, long, long);
    public boolean allDifferent$mSc$sp(short, short, short);
    public boolean allDifferent$mVc$sp(scala.runtime.BoxedUnit, scala.runtime.BoxedUnit, scala.runtime.BoxedUnit);
    */

    def allDifferent2[@specialized(Boolean, Byte) T] (x: T, y: T, z: T) = x != y && x != z && y != z
    // public <T extends java/lang/Object> boolean allDifferent2(T, T, T);
    // public boolean allDifferent2$mZc$sp(boolean, boolean, boolean);
    // public boolean allDifferent2$mBc$sp(byte, byte, byte);

    // 9.
    println("------------------------------  practice 9 -------------------------");
    // Range.foreach[@specialized(Unit) T] ...
    // public final <U extends java/lang/Object> void foreach(scala.Function1<java.lang.Object, U>);
    // public final void foreach$mVc$sp(scala.Function1<java.lang.Object, scala.runtime.BoxedUnit>);

    // trait Function1[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef 

    // 10.
    println("------------------------------  practice 10 -------------------------");
    def factorial(a: Int) = {
        assert( a >= 0)
        println(a)
    }

    factorial(1)

    println("=======================================================");
}
