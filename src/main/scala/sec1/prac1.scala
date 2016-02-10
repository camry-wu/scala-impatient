// 基础

// 1.
// scala REPL has no feedback while print TAB

// 2.
// import scala.math._
// pow(sqrt(3), 2) = 2.9999...6

// 3.
// res is val

// 4.
// "crazy" * 3 = crazycrazycrazy

// 5.
// 10 max 2 = 10.max(2) = 10
// scala.math.max

// 6.
// val a = BigInt(2)
// val b = a.pow(1024)

// 7. 
// import scala.math.BigInt._
// probablePrime(100, scala.util.Random)

// 8.
// val b = probablePrime(100, scala.util.Random)
// val c = b.toString(36)

// 9.
// "crazy".head or "crazy"(0)
// "crazy".last or "crazy"("crazy".length - 1)

// 10.
// "camry".take(3) = "cam"          "camry".substring(0, 3)
// "camry".takeRight(3) = "mry"     "camry".substring("camry".length - 3)
// "camry".drop(3) = "ry"           "camry".substring(3)
// "camry".dropRight(3) = "ca"      "camry".substring(0, "camry".length - 3)

// good: more readable, more security(donot throw IndexOutOfBound error)
// bad: no, maybe low performance? or donot throw error
