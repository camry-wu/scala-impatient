package sec20
// Actor

import akka.actor.{Actor, ActorLogging, Props, ActorRef, ActorSystem}
import scala.collection._

// 1.
class AvgActor extends Actor with ActorLogging {

    private var sum: Int = 0
    private var account: Int = 0
    private var index: Int = -1
	private var signal: Int = AvgActor.WORKER_NUM
	val workerActors = init()

    def receive = {
        case x: Int => {
			account += 1
			index += 1
			if (index >= AvgActor.WORKER_NUM) index = 0
			workerActors(index) ! x
		}
        case AvgActor.Eof => {
			workerActors.foreach(_ ! AvgActor.Eof)
		}
        case AvgActor.Sum(x) => {
			sum += x
			signal -= 1
			if (signal <= 0) {
				// finish
				val avg = sum / account
				log.info(s"receive $account Int, avg is: $avg !")
				context.system.shutdown
			}
		}
    }

    private def init(): mutable.ArrayBuffer[ActorRef] = {
        val result = mutable.ArrayBuffer[ActorRef]()
        for (i <- 1 to AvgActor.WORKER_NUM) result += context.actorOf(Props[AvgWorkerActor], "avgWorkerActor" + i)
        //log.info(result.size.toString())
        result
    }
}

object AvgActor {
    val WORKER_NUM = 4
    val props = Props[AvgActor]
    case object Eof
    case class Sum(sum: Int)
}

class AvgWorkerActor extends Actor with ActorLogging {
    import scala.util.Random

    private var sum: Int = 0

    // 在此处生成随机数并加到总数中
    // 完成后把总数发回去
    def receive = {
        case x: Int => {
			sum += Random.nextInt(x)
		}
        case AvgActor.Eof => { 
			sender ! AvgActor.Sum(sum)
		}
    }
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
	val begin = System.currentTimeMillis
    val actorSystem = ActorSystem("AvgSystem")
    val actor = actorSystem.actorOf(AvgActor.props)

	for (i <- 1 to 1000000) actor ! i
	actor ! AvgActor.Eof

    actorSystem.awaitTermination()
    actorSystem.shutdown()
	val end = System.currentTimeMillis
	println("use time: " + (end - begin))
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
