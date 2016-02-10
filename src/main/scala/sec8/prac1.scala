package sec8
// 继承

// 1.
class BankAccount(initialBalance: Double) {
    private var balance = initialBalance
    def deposit(amount: Double) = { balance += amount; balance }
    def withdraw(amount: Double) = { balance -= amount; balance }
    def current(): Double = { balance }
    override def toString = "Account: %f.".format(current)
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    override def deposit(amount: Double) = { super.deposit(amount); super.withdraw(1) }
    override def withdraw(amount: Double) = { super.withdraw(amount); super.withdraw(1) }
}

// 2.
class SavingsAccount(initialBalance: Double) extends BankAccount (initialBalance) {
    val rate = 0.003
    var exemptCount = 3

    def serviceFee() {
        if (exemptCount > 0) {
            exemptCount -= 1
        } else {
            super.withdraw(1)
        }
    }

    def earnMonthlyInterest() = { super.deposit(current * rate); exemptCount = 3 }

    override def deposit(amount: Double) = {
        super.deposit(amount)
        serviceFee()
        current
    }

    override def withdraw(amount: Double) = {
        super.withdraw(amount)
        serviceFee()
        current
    }
}

// 3.

// 4.
abstract class Item {
    def price: Double
    def description: String
    override def toString = "%5.2f : %s".format(price, description)
}

class SimpleItem(p: Double, desc: String) extends Item {
    val price: Double = p               // val price will provide a price method
    val description: String = desc      // val description will provide a description method
}

import scala.collection.mutable.ArrayBuffer
class Bundle extends Item {
    val items: ArrayBuffer[Item] = new ArrayBuffer[Item]()
    def addItem(item: Item): Bundle = {
        items += item
        this
    }

    def price: Double = {
        var sum: Double = 0.0
        for (item <- items) {
            sum = sum + item.price
        }
        sum
    }

    def description: String = {
        val desc: StringBuilder = new StringBuilder("Bundle with: ")
        for (item <- items) {
            desc ++= " | " ++= item.description
        }
        desc.toString
    }
}

// 5.
class Point(val x: Int, val y: Int) {
    override def toString = "(%d, %d)".format(x, y)
}

class LabeledPoint(val label: String, override val x: Int, override val y: Int) extends Point(x, y) {
    override def toString = "(%s, %d, %d)".format(label, x, y)
}

// 6.
abstract class Shape {
    def centerPoint: Point
}

class Rectangle(val lefttop: Point, val length: Int, val width: Int) extends Shape {
    def centerPoint: Point = {
        val x = lefttop.x + length / 2
        val y = lefttop.y - width / 2
        new Point(x, y)
    }
    override def toString = "LeftTop(%s), Length: %d, Width: %d".format(lefttop, length, width)
}

class Circle(val centerPoint: Point, val radius: Int) extends Shape {
    override def toString = "Center(%s), Radius: %d".format(centerPoint, radius)
}

// 7.
import java.awt.{Point => JavaPoint}
import java.awt.{Rectangle => JavaRectangle}
class Square(val topleft: JavaPoint, width: Int) extends JavaRectangle(topleft.x, topleft.y, width, width) {
    def this() {
        this(new JavaPoint(0, 0), 0)
    }

    def this(w: Int) {
        this(new JavaPoint(0, 0), w)
    }

    override def toString = "TopLeft(%s), Width: %d".format(topleft, width)
}

// 8.
class Person(val name: String) {
    override def toString = getClass.getName + "[name=" + name + "]"
}

class SecretAgent(codename: String) extends Person(codename) {
    override val name = "secret"
    override val toString = "secret"
}

// 9.
class Creature {
    //val range: Int = 10
    def range: Int = 10
    val env: Array[Int] = new Array[Int] (range)
    override def toString = "env array capacity: %d".format(env.length)
}

class Ant extends Creature {
    //override val range = 2
    override def range = 2
}

// 10.
import scala.collection.immutable.Stack
class MyStack[A] (el: List[A]) extends Stack[A] (el) {
}

object PracTest extends App {
    println ("sec8.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    val account = new CheckingAccount(100)
    println(account)
    account.deposit(30);
    println(account)
    account.withdraw(20);
    println(account)

    // 2.
    println("------------------------------  practice 2 -------------------------");
    // month 1
    val ac2 = new SavingsAccount(100)
    ac2.deposit(30);
    ac2.withdraw(20);
    ac2.deposit(10);
    println(ac2)
    ac2.withdraw(50);
    println(ac2)
    // month 2
    ac2.earnMonthlyInterest();
    ac2.deposit(30);
    ac2.withdraw(20);
    ac2.deposit(10);
    println(ac2)
    ac2.withdraw(50);
    println(ac2)

    // 3.
    println("------------------------------  practice 3 -------------------------");
    // do nothing

    // 4.
    println("------------------------------  practice 4 -------------------------");
    val item1 = new SimpleItem(28.9, "Cap")
    val item2 = new SimpleItem(59.2, "Shoes")
    val item3 = new SimpleItem(88.3, "Shirt")
    val bundle = new Bundle
    bundle.addItem(item1).addItem(item2).addItem(item3)
    println(item1)
    println(item2)
    println(item3)
    println(bundle)

    // 5.
    println("------------------------------  practice 5 -------------------------");
    val p1 = new Point(27, 38)
    val p2 = new LabeledPoint("danny", 59, 24)
    println(p1)
    println(p2)

    // 6.
    println("------------------------------  practice 6 -------------------------");
    val rec = new Rectangle(new Point(25,26), 30, 10)
    val circle = new Circle(new Point(33,34), 20)
    println(rec)
    println(rec.centerPoint)
    println(circle)
    println(circle.centerPoint)

    // 7.
    println("------------------------------  practice 7 -------------------------");
    val squ1 = new Square()
    val squ2 = new Square(30)
    val squ3 = new Square(new JavaPoint(2, 3), 30)
    println(squ1)
    println(squ2)
    println(squ3)

    // 8.
    println("------------------------------  practice 8 -------------------------");
    // Person.name      camry
    // SecretAgent.name secret
    val p = new Person("camry")
    val s = new SecretAgent("danny")
    println(p)
    println(s)

    // 9.
    println("------------------------------  practice 9 -------------------------");
    // scalac -Xcheckinit
    // 1. super class use val range to init
    //      if range be override by subclass
    //      the env will be initial as Array[Int](0)
    // 2. super class use def range to init
    //      if range be override by subclass as val
    //      the env will be initial as Array[Int](0)
    // 3. super class use def range to init
    //      if range be override by subclass as def
    //      the env will be initial as Array[Int](2)
    val creature = new Creature
    val ant = new Ant
    println(creature)
    println(ant)

    // 10.
    println("------------------------------  practice 10 -------------------------");
    // class Stack[A] protected (protected val elems: List[A])
    // only sub class can visit this constructor and elems ?
    val mystack = new MyStack[Int](List(1,2,3))
    //val stack = new Stack[Int](List(1,2,3))     // compile error
}
