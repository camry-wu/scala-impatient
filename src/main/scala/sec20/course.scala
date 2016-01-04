package sec20
package course
// 注意，scala.2.10 版后改为 akka 的 actor 库， 这里使用 akka actor 库

import akka.actor.{Actor, ActorLogging, Props}

// 1. 创建和启动 Actor
class PingActor extends Actor with ActorLogging {
	import PingActor._
  
	var counter = 0
	val pongActor = context.actorOf(PongActor.props, "pongActor")

	def receive = {
		case Initialize => 
			log.info("In PingActor - starting ping-pong")
			pongActor ! PingMessage("ping")	
		case PongActor.PongMessage(text) =>
			log.info("In PingActor - received message: {}", text)
			counter += 1
			if (counter == 3) context.system.shutdown()
			else sender() ! PingMessage("ping")
	}
}

object PingActor {
  val props = Props[PingActor]
  case object Initialize
  case class PingMessage(text: String)
}

class PongActor extends Actor with ActorLogging {
	import PongActor._

	def receive = {
		case PingActor.PingMessage(text) => 
			log.info("In PongActor - received message: {}", text)
			sender() ! PongMessage("pong")
	}
}

object PongActor {
	val props = Props[PongActor]
	case class PongMessage(text: String)
}

// 2. 发送消息

// 3. 接收消息

// 4. 向其他 Actor 发送消息

// 5. 消息通道

// 6. 同步消息和 Future

// 7. 共享线程

// 8. Actor 的生命周期

// 9. 将多个 Actor 链接在一起

// 10. Actor 的设计

object CourseTest extends App {
    println ("sec20.course.CourseTest")

    // 1.
    println("------------------------------  section 1 -------------------------");
	import akka.actor.ActorSystem
	val system = ActorSystem("MyActorSystem")
	val pingActor = system.actorOf(PingActor.props, "pingActor")
	pingActor ! PingActor.Initialize
	// This example app will ping pong 3 times and thereafter terminate the ActorSystem - 
	// see counter logic in PingActor
	system.awaitTermination()

    // 2.
    println("------------------------------  section 2 -------------------------");

    // 3.
    println("------------------------------  section 3 -------------------------");

    // 4.
    println("------------------------------  section 4 -------------------------");

    // 5.
    println("------------------------------  section 5 -------------------------");

    // 6.
    println("------------------------------  section 6 -------------------------");

    // 7.
    println("------------------------------  section 7 -------------------------");

    // 8.
    println("------------------------------  section 8 -------------------------");

    // 9.
    println("------------------------------  section 9 -------------------------");

    // 10.
    println("------------------------------  section 10 -------------------------");
}
