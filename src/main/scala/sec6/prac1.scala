package sec6

// 1.
object Conversions {
    def inchesToCentimeters(src: Double) : Double = {
        val res = src * 2.54
        res
    }
    def gallonsToLiters(src: Double) : Double = {
        val res = src * 3.7854118
        res
    }
    def milesToKilometers(src: Double) : Double = {
        val res = src * 1.609344
        res
    }
}

// 2.
abstract class UnitConversion {
    def inchesToCentimeters(src: Double) : Double
    def gallonsToLiters(src: Double) : Double
    def milesToKilometers(src: Double) : Double
}
class UnitConversionImpl extends UnitConversion {
    override def inchesToCentimeters(src: Double) : Double = {
        val res = src * 2.54
        res
    }
    override def gallonsToLiters(src: Double) : Double = {
        val res = src * 3.7854118
        res
    }
    override def milesToKilometers(src: Double) : Double = {
        val res = src * 1.609344
        res
    }
}

// 3.
class Origin extends java.awt.Point {
}

// 4.
object Origin {
    def apply(x: Int, y: Int): Origin = {
        val res = new Origin
        res.move(x, y)
        res
    }
}

// 5.

// 6.
import scala.math.BigInt._
class Card(val value: Int, val design: CardDesign.Value) {
    override def toString = "%s-%d".format(design, value)
}
object Card {
    def apply(): Card = {
        val value = (probablePrime(10, scala.util.Random) % 13 + 1).intValue
        val design = CardDesign((probablePrime(10, scala.util.Random) % 13 % 4).intValue)
        val card = new Card(value, design)
        card
    }
}

object CardDesign extends Enumeration {
    val HeiTao = Value(0, "HEI")
    val HongTao = Value(1, "HONE")
    val MeiHua = Value(2, "MEI")
    val FangKuai = Value(3, "◆")
}

// 7.

object RGBX extends Enumeration {
    val X0 = Value(0x000000, "0")     // 000
    val X1 = Value(0x0000ff, "1")     // 001
    val X2 = Value(0x00ff00, "2")     // 010
    val X3 = Value(0x00ffff, "3")     // 011
    val X4 = Value(0xff0000, "4")     // 100
    val X5 = Value(0xff00ff, "5")     // 101
    val X6 = Value(0xffff00, "6")     // 110
    val X7 = Value(0xffffff, "7")     // 111
}

// 8.

object PracTest extends App {
    println ("sec6.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    println(Conversions.inchesToCentimeters(10))
    println(Conversions.gallonsToLiters(10))
    println(Conversions.milesToKilometers(10))

    // 2.
    println("------------------------------  practice 2 -------------------------");
    val uci: UnitConversionImpl = new UnitConversionImpl
    println(uci.inchesToCentimeters(10))
    println(uci.gallonsToLiters(10))
    println(uci.milesToKilometers(10))

    // 3.
    println("------------------------------  practice 3 -------------------------");
    val point1: Origin = new Origin
    val point2: Origin = new Origin
    point1.move(5, 8)
    point2.move(5, 8)
    println (point1.equals(point2))

    // 4.
    println("------------------------------  practice 4 -------------------------");
    val origin = Origin(8, 9)
    println (origin)

    // 5.
    println("------------------------------  practice 5 -------------------------");
    val arg: Array[String] = args.reverse
    for (elem <- arg) { print(elem); print(" "); }
    println("")

    // 6.
    println("------------------------------  practice 6 -------------------------");
    println(CardDesign.HeiTao)
    println(CardDesign.HongTao)
    println(CardDesign.MeiHua)
    println(CardDesign.FangKuai)

    // 7.
    println("------------------------------  practice 7 -------------------------");
    def checkRedCard(card: Card): Boolean = {
        card.design match {
            case CardDesign.HeiTao => false
            case CardDesign.MeiHua => false
            case CardDesign.HongTao => true
            case CardDesign.FangKuai => true
        }
    }
    val card1 = Card()
    val card2 = Card()
    val card3 = Card()
    val card4 = Card()

    println(card1 + ": is red? " + checkRedCard(card1))
    println(card2 + ": is red? " + checkRedCard(card2))
    println(card3 + ": is red? " + checkRedCard(card3))
    println(card4 + ": is red? " + checkRedCard(card4))

    // 8.
    println("------------------------------  practice 8 -------------------------");
    println(RGBX.X0)
    println(RGBX.X1)
    println(RGBX.X2)
    println(RGBX.X3)
    println(RGBX.X4)
    println(RGBX.X5)
    println(RGBX.X6)
    println(RGBX.X7)
}
