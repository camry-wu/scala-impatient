package sec5
// 类

// 1.
class Counter {
    private var value = 0
    def increment() {
        if (value < Int.MaxValue)
            value += 1
    }
    def current() = value
}

object Counter {
    def apply(v: Int) = {
        val ct = new Counter()
        ct.value = v
        ct
    }
}

// 2.
class BankAccount(val balance: Int) {
    def deposit(v: Int) : BankAccount = { val newOne = new BankAccount(balance + v); newOne }
    def withdraw(v: Int) : BankAccount = { val newOne = new BankAccount(balance - v); newOne }
    override def toString = "Bank Account: " + balance;
}

// 3.
class Time(h: Int, m: Int) {
    val hours: Int = if (h < 0) 0 else if (h > 23) 23 else h
    val minutes: Int = if (m < 0) 0 else if (m > 59) 59 else m

    def before(other: Time): Boolean = {
        val res: Boolean =
            if (hours < other.hours || (hours == other.hours && minutes < other.minutes)) true
            else false
        res
    }
    override def toString = "%02d:%02d".format(hours, minutes)
}

// 4.
class Time2(h: Int, m: Int) {
    val minutes: Int = (if (h < 0) 0 else if (h > 23) 23 else h) * 60 + (if (m < 0) 0 else if (m > 59) 59 else m)

    def before(other: Time2): Boolean = {
        val res: Boolean =
            if (minutes < other.minutes) true
            else false
        res
    }
    override def toString = "%02d:%02d".format(minutes / 60, minutes % 60)
}

// 5.
import scala.beans.BeanProperty
class Student(@BeanProperty var id: Long, @BeanProperty var name: String) {
    override def toString = "id: %d, name: %s".format(id, name)
}

// 6.
class Person(a: Int) {
    val age = if (a < 0) 0 else a
}

// 7.
class Person2(str: String) {
    val Person2(firstName, lastName) = str
}
object Person2 {
    def unapply(input: String) = {
        val pos = input.indexOf(" ")
        if (pos == -1) None
        else Some((input.take(pos), input.drop(pos + 1)))
    }
}

// 8.
class Car(val provider: String, val sku: String, val year: Int, val number: String) {
    def this(p: String, s: String) {
        this(p, s, -1, "")
    }

    def this(p: String, s: String, y: Int) {
        this(p, s, y, "")
    }

    def this(p: String, s: String, n: String) {
        this(p, s, -1, n)
    }

    override def toString = "Provider: %s, SKU: %s, Year: %d, Number: %s".format(provider, sku, year, number)
}

// 9.
// public class Car {
//     private String provider;
//     private String sku;
//     private int year;
//     private String number;
// 
//     // getter and setter
//     //...
// 
//     public Car(String p, String s, int y, String n) { ... }
//     public Car(String p, String s, int y) { ... }
//     public Car(String p, String s, String n) { ... }
//     public Car(String p, String s) { ... }
// }

// 10.
class Employee(val name: String, var salary: Double) {
    def this() { this("John Q. Public", 0.0) }

    override def toString = "Name: %s, Salary: %f".format(name, salary)
}

class Employee2() {
    val name: String = "John Q. Public"
    var salary: Double = 0.0

    override def toString = "Name: %s, Salary: %f".format(name, salary)
}

object PracTest extends App {
    println ("sec5.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    val ct = Counter(Int.MaxValue)
    println(ct.current)
    ct.increment
    println(ct.current)

    // 2.
    println("------------------------------  practice 2 -------------------------");
    println(new BankAccount(50))
    println(new BankAccount(50).deposit(200))
    println(new BankAccount(50).deposit(20))

    // 3.
    println("------------------------------  practice 3 -------------------------");
    val t1: Time = new Time(24, 80)
    val t2: Time = new Time(-4, -5)
    println(t1)
    println(t2)
    println("t1.before(t2): " + t1.before(t2))
    println("t2.before(t1): " + t2.before(t1))

    // 4.
    println("------------------------------  practice 4 -------------------------");
    val t3: Time2 = new Time2(24, 80)
    val t4: Time2 = new Time2(-4, -5)
    println(t3)
    println(t4)
    println("t3.before(t4): " + t3.before(t4))
    println("t4.before(t3): " + t4.before(t3))

    // 5.
    println("------------------------------  practice 5 -------------------------");
    val student: Student = new Student(0, "camry")
    student.setId(1)
    student.setName("danny")
    println(student)

    // 6.
    println("------------------------------  practice 6 -------------------------");
    val person1: Person = new Person(-8)
    val person2: Person = new Person(8)
    println(person1.age)
    println(person2.age)

    // 7.
    println("------------------------------  practice 7 -------------------------");
    val person3: Person2 = new Person2("camry danny")
    println(person3.firstName)
    println(person3.lastName)

    // 8.
    println("------------------------------  practice 8 -------------------------");
    val car1: Car = new Car("DZ", "Tu-Guan")
    val car2: Car = new Car("XD", "Yue-Dong", 1984)
    val car3: Car = new Car("FT", "Focus", "NAH-759")
    val car4: Car = new Car("LK", "Lin-Keng", 2010, "8356")
    println(car1)
    println(car2)
    println(car3)
    println(car4)

    // 9.
    println("------------------------------  practice 9 -------------------------");
    // define of scala is simple

    // 10.
    println("------------------------------  practice 10 -------------------------");
    println(new Employee());
    println(new Employee2());
    // the Employee is better than Employee2 I think
}
