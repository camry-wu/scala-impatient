package sec17

// 1.
class Pair[T, S] (val t: T, val s: S) {
    def swap(): Pair[S, T] = {
        val ret = new Pair[S, T] (s, t)
        ret
    }

    override def toString() = { "Pair[T, S](%s, %s)".format(t.toString, s.toString) }
}

// 2.
package mutable {
    class Pair[T] (var t1: T, var t2: T) {
        def swap() {
            val tmp = t1
            t1 = t2
            t2 = tmp
        }

        override def toString() = { "Pair[T](%s, %s)".format(t1.toString, t2.toString) }
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
    println ("sec17.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    val pair1 = new Pair(true, "b")
    println(pair1)
    println(pair1.swap())

    // 2.
    println("------------------------------  practice 2 -------------------------");
    val pair2 = new mutable.Pair(true, false)
    println(pair2)
    pair2.swap()
    println(pair2)

    // 3.
    println("------------------------------  practice 3 -------------------------");
    def swap[T, S](p: Pair[T, S]): Pair[S, T] = {
        val ret = new Pair[S, T](p.s, p.t)
        ret
    }

    val pair3 = new Pair(true, 1L)
    println(pair3)
    println(swap(pair3))

    // 4.
    println("------------------------------  practice 4 -------------------------");
    // 17.3节中，想把 Pair[Person] 的第一个组件替换成 Student，为什么不需要给 replaceFirst 方法定一个下界？
    // 因为 Student 是 Person 的子类，用子类来替换超类型，仍然可以返回 Pair[Person]
    // 可用超类来替换子类的话，就需要推断出将原来的 Pair[Student] 改为超类 Pair[Person]

    // 5.
    println("------------------------------  practice 5 -------------------------");
    // 为何 RichInt 实现的是 Comparable[Int] 而不是 Comparable[RichInt]
    // Int 可以被隐式转换为 RichInt，因此使用 Int 的时候可以利用 RichInt 的特性
    // 若实现的是 Comparable[RichInt]，则就不满足 Int <% Comparable[Int] 的条件
    // 没法用隐式转换成 RichInt 来解决 17.4 节中的问题了

    // 6.
    println("------------------------------  practice 6 -------------------------");
    def middle[T](iter: Iterable[T]): T = {
        val size = iter.size
        val idx = size / 2
        val mid = Math.min(idx + 1, size - 1)
        iter.take(mid).last
    }

    println(middle("World"))
    println(middle(Array(1, 2, 3)))

    // 7.
    println("------------------------------  practice 7 -------------------------");

    // 8.
    println("------------------------------  practice 8 -------------------------");

    // 9.
    println("------------------------------  practice 9 -------------------------");

    // 10.
    println("------------------------------  practice 10 -------------------------");
}
