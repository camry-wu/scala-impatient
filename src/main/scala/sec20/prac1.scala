package sec20
// Actor

import akka.actor.{Actor, ActorLogging, Props, ActorRef, ActorSystem}

// 1.
class AvgActor extends Actor with ActorLogging {
	//val workerActor = context.actorOf(AvgWorkerActor.props, "avgWorkerActor")
    def receive = {
        case x: Int => {}
        case AvgActor.Eof => {}
        case _ => {}
    }
}

object AvgActor {
    val props = Props[AvgActor]
    case object Eof
}

// 2.

// 3.

// 4.

// 5.

// 6.

// 7.

// 8.

// 9.

// 10.

object PracTest1 extends App {
    // 1.
    println("------------------------------  practice 1 -------------------------");
}

object PracTest2 extends App {
    // 2.
    println("------------------------------  practice 2 -------------------------");
}

object PracTest3 extends App {
    // 3.
    println("------------------------------  practice 3 -------------------------");
}

object PracTest4 extends App {
    // 4.
    println("------------------------------  practice 4 -------------------------");
}

object PracTest5 extends App {
    // 5.
    println("------------------------------  practice 5 -------------------------");
}

object PracTest6 extends App {
    // 6.
    println("------------------------------  practice 6 -------------------------");
}

object PracTest7 extends App {
    // 7.
    println("------------------------------  practice 7 -------------------------");
}

object PracTest8 extends App {
    // 8.
    println("------------------------------  practice 8 -------------------------");
}

object PracTest9 extends App {
    // 9.
    println("------------------------------  practice 9 -------------------------");
}

object PracTest10 extends App {
    // 10.
    println("------------------------------  practice 10 -------------------------");
}
