package sec7

// 1.
package com.horstmann.impatient {
    // cannot see com.horstmann._
    // cannot see com._
}

package com {
    package horstmann {
        package impatient {
            // can see com._ and com.horstmann._
        }
    }
}

// 2.

// 3.
import scala.math._
package object random {
    val a: Int = 1664525
    val b: Int = 1013904223
    val n: Int = 32
}

package random {
    class Gen(var seed: Int) {
        var intValue: Double = seed
        var doubleValue: Double = seed

        def nextInt(): Int = {
            intValue = (intValue * a + b) % (pow(2, n))
            intValue.intValue
        }

        def nextDouble(): Double = {
            doubleValue = (doubleValue * a + b) % (pow(2, n))
            doubleValue
        }

        def setSeed(seed: Int): Unit = {
            this.seed = seed
            intValue = seed
            doubleValue = seed
        }
    }
}

// 4.

// 5.

// 6.

// 7.

// 8.

// 9.

// 10.

object PracTest extends App {
    println ("sec7.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");

    // 2.
    println("------------------------------  practice 2 -------------------------");

    // 3.
    println("------------------------------  practice 3 -------------------------");
    val gen = new random.Gen(1) // or sec7.random.Gen(1)
    println(gen.nextInt)
    println(gen.nextInt)
    gen.setSeed(2)
    println(gen.seed)
    println(gen.nextDouble)
    println(gen.nextDouble)

    // 4.
    println("------------------------------  practice 4 -------------------------");
    // package can be defined in different file
    // but package object should be defined in one file

    // 5.
    println("------------------------------  practice 5 -------------------------");
    // private[com] def giveRaise(rate: Double)
    // giveRaise can be visit only in com package

    // 6.
    println("------------------------------  practice 6 -------------------------");
    import java.util.{HashMap => JavaHashMap}
    import scala.collection.mutable.{HashMap => ScalaHashMap}
    def cp[K, V](src: JavaHashMap[K, V]): ScalaHashMap[K, V] = {
        var res: ScalaHashMap[K, V] = new ScalaHashMap[K, V]
        val x = src.keySet.iterator
        while (x.hasNext()) {
            val k = x.next()
            val v = src.get(k)
            res(k) = v
        }
        res
    }

    var map1 = new JavaHashMap[String, Int] ()
    map1.put("camry", 1)
    map1.put("danny", 2)
    var map2 = cp(map1)
    println(map2)

    // 7.
    println("------------------------------  practice 7 -------------------------");
    // do nothing

    // 8.
    println("------------------------------  practice 8 -------------------------");
    // import all java._ and javax._, and javax._ will override java._.
    // I don't link this

    // 9.
    println("------------------------------  practice 9 -------------------------");
    import java.lang.System
    import scala.io.StdIn
    val username: String = System.getProperty("user.name")
    println(username + ", Please input your password: ")
    val pass: String = StdIn.readLine()
    if ("secret".equals(pass)) {
        Console.out.println("Welcome " + username + "!")
    } else {
        Console.err.println("Sorry, username and password unmatched!")
    }

    // 10.
    println("------------------------------  practice 10 -------------------------");
    // scala._ override: Boolean, Byte, Char, Double, Float, Int, Long, Short, StringBuilder
}
