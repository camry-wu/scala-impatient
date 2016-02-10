package sec17
// 类型参数

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
    class Pair[T] (var first: T, var second: T) {
        def swap() {
            val tmp = first
            first = second
            second = tmp
        }

        override def toString() = { "Pair[T](%s, %s)".format(first.toString, second.toString) }

        // 第 8 题，以下定义编译会出错 first 是 T，不能赋值为 R
        // def replaceFirst[R >: T](newFirst: R) { first = newFirst }

        // 以下定义可以编译通过，返回了一个新的 Pair[R]，此时 first 和 second 都是 R
        def replaceFirst[R >: T](newFirst: R) = new Pair[R](newFirst, second)
    }
}

// 3.

// 4.

// 5.

// 6.

// 7.
class Person(val name: String) {
    override def toString = "P-[%s]".format(name)
}

class Student(name: String) extends Person(name) {
    override def toString = "S-[%s]".format(name)
}

class SubStudent(name: String) extends Student(name) {
    override def toString = "SubS-[%s]".format(name)
}

// 8.

// 9.

// 10.
package mutable {
    class NPair[S, T] (var first: S, var second: T) {
        def swap(implicit ev: S =:= T) = {
            val tmp = first.asInstanceOf[T]
            first = second.asInstanceOf[S]
            second = tmp
        }

        override def toString() = { "NPair[T](%s, %s)".format(first.toString, second.toString) }
    }
}

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
    // 查看 Iterable[+A] 特质。哪些方法使用了类型参数 A ?
    // "++:", "/:", ":\", aggregate, collect, collectFirst, copyToArray
    // copyToBuffer, count, dropWhile, exists, filter, filterNot,
    // find, flatMap, foldLeft, foldRight, forall, foreach, groupBy
    // map, maxBy, minBy, partition, reductLeft, reduceLeftOption,
    // reductRight, reductRightOption, sameElements, scan,
    // scanLeft, scanRight, span, takeWhile, 
    // transpose, unzip, unzip3, withFilter, zipAll

    // 协变表示与 A 按同样的方向型变，Iterable[A 子类型] 是 Iterable[A] 的子类型
    // 一般来说协变类型应当在返回值这个点上，逆变类型在参数值这个协变点上

    // 为何在这些方法中类型参数位于协变点？
    // 这些方法的参数往往都是一个函数，A 是函数中的参数；或者隐式调用中的参数
    // zipAll 有点特殊：zipAll[B](that: Iterable[B], thisElem: A, thatElem: B): Iterable[(A, B)]
    // thisElem 表示当左列表比右列表短时，左边的缺省值

    // 表示 Iterable[Person].zipAll(that, new Student(), thatElem) 是可行的？
    val list7:List[Person] = List(new Person("camry"), new Person("danny"))
    val list72:List[Int] = List(1, 2, 3)

    println(list7.toString)
    println(list72.toString)

    val list73 = list7.zipAll(list72, new Student("other"), 99) // 推断出 A 是 Person?
    println(list73.toString)

    // val list74:List[Student] = List(new Person("camry"), new Person("danny"))   // 编译不过
    val list74:List[Student] = List(new Student("camry"), new Student("danny"))

    println(list74.zipAll(list72, new Person("other"), 99)) // 推断出 A 是 Person

    // 8.
    println("------------------------------  practice 8 -------------------------");
    // 以下定义编译会出错 first 是 T，不能赋值为 R

    // 9.
    println("------------------------------  practice 9 -------------------------");
    // TODO 未明白题意

    // 10.
    println("------------------------------  practice 10 -------------------------");
    val p10 = new mutable.NPair("camry", true)
    // p10.swap       // compile error
    println(p10)

    val p11 = new mutable.NPair("camry", "danny")
    p11.swap            // compile ok, but p11.swap() cannot compile
    println(p11)

    val p12 = new mutable.NPair(true, false)
    p12.swap
    println(p12)
}
