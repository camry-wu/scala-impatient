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
@deprecated(message="class") class Anno @deprecated(message="constructor") (var value: String) {
    @deprecated(message="member") val name = ""
    @deprecated(message="method") def a() {}

    def b(@deprecated(message="var") src: String) {
        val a: Int = (6 + 5): @deprecated(message="exp")
    }

    def c[@deprecated(message="var") T] (a: T) {
    }
}

// 3.

// 4.

// 5.

// 6.

// 7.

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

    // 3.
    println("------------------------------  practice 3 -------------------------");

    // 4.
    println("------------------------------  practice 4 -------------------------");

    // 5.
    println("------------------------------  practice 5 -------------------------");

    // 6.
    println("------------------------------  practice 6 -------------------------");

    // 7.
    println("------------------------------  practice 7 -------------------------");

    // 8.
    println("------------------------------  practice 8 -------------------------");

    // 9.
    println("------------------------------  practice 9 -------------------------");

    // 10.
    println("------------------------------  practice 10 -------------------------");
}
