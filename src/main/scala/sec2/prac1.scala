package sec2
// 控制结构和函数

class Prac1 {
}

object Prac1 extends App {
    println ("sec2.Prac1")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    def getSignNum(a: Int): Int = {
        val sign =
            if (a > 0) 1 
            else if (a == 0) 0
            else -1

        sign
    }

    println ("getSignNum(5) = " + getSignNum(5))
    println ("getSignNum(0) = " + getSignNum(0))
    println ("getSignNum(-8) = " + getSignNum(-8))

    // 2. {} value is Unit, type is Unit ?

    // 3.
    // var x: Unit = Unit
    // var y: Int = 0
    // x = y = 1            // then x = Unit, y = 1

    // 4.
    println("------------------------------  practice 4 -------------------------");
    for (a <- (1 to 10).reverse) println (a)

    // 5.
    println("------------------------------  practice 5 -------------------------");
    def countdown(n: Int) {
        if (n > 0)
            for (a <- (0 to n).reverse) println (a)
        else
            println ("Exception: n <= 0");
    }

    countdown(5);
    countdown(-1);

    // 6.
    println("------------------------------  practice 6 -------------------------");
    def letterProduct(str: String) : BigInt = {
        val opt: Option[String] = Option(str)

        var sum = BigInt(1)

        opt match {
            case Some(s) => for (a <- s) sum *= a.toInt
            case None => println ("Exception: null string!")
        }
        sum
    }

    println (letterProduct("Hello"))
    println (letterProduct(null))

    // 7.
    // 8.
    println("------------------------------  practice 7, 8 -------------------------");
    def product(str: String) : BigInt = {
        val opt: Option[String] = Option(str)

        var sum = BigInt(1)

        opt match {
            case Some(s) => sum = s.toBuffer[Char].map(_.toInt).map(BigInt(_)).product
            case None => println ("Exception: null string!")
        }
        sum
    }

    println (product("Hello"))

    // 9.
    println("------------------------------  practice 9 -------------------------");
    def product2(str: String) : BigInt = {
        val opt: Option[String] = Option(str)

        var sum: BigInt = null

        opt match {
            case Some(s) => sum = if(s.isEmpty) 0 else recursiveProductStr(BigInt(1), s)
            case None => println ("Exception: null string!")
        }
        sum
    }

    def recursiveProductStr(prod: BigInt, str: String): BigInt = {
        if(str.isEmpty()) prod
        else recursiveProductStr(prod * BigInt(str.head.toInt), str.tail)
    }

    println (product2("Hello"))

    // 10.
    println("------------------------------  practice 10 -------------------------");
    def mypow(x: BigInt, n: Int) : BigInt = {
        if (n < 0) 1 / mypow(x, -1 * n)
        else if (n == 0) 1
        else if (n % 2 == 0) mypow(x, n / 2) * mypow(x, n / 2)
        else x * mypow(x, n - 1)
    }

    println (mypow(2, 3))
    println (mypow(3, 3))
    println (mypow(2, 1024))
    println (BigInt(2)pow(1024))
}
