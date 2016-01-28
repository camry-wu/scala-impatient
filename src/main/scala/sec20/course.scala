package sec20
package course
// 注意，scala.2.10 版后改为 akka 的 actor 库， 这里使用 akka actor 库
// 学习系列文章：   http://rerun.me/2014/09/19/akka-notes-actor-messaging-1/
// http://it.deepinmind.com/akka/2014/10/22/akka-notes-logging-and-testing.html
// --------------
// actor 生命周期:
// 通过构造函数创建 Actor
// 调用它的 preStart 方法
// 接收消息时，调用他的 receive 方法
// 最后调用 postStop 方法将 Actor 置为终结状态

// --------------
// 另外：执行 activator 的 about 命令可以自动 resolve 依赖库

import akka.actor.{Actor, ActorLogging, Props, ActorRef}

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
			if (counter == 3) {} // context.system.shutdown()   // 如果调用，会触发 system 关闭事件，后面的 schedule 任务不再执行
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
import scala.util.Random

object TeacherProtocol {
    case class QuoteRequest()
    case class QuoteResponse(quoteString: String)
}

class TeacherActor extends Actor with ActorLogging {
    val quotes = List(
        "Moderation is for cowards",
        "Anything worth doing is worth overdoing",
        "The trouble is you think you have time",
        "You never gonna know if you never even try"
    )

    def receive = {
        case TeacherProtocol.QuoteRequest => {
            val quoteResponse = TeacherProtocol.QuoteResponse(quotes(Random.nextInt(quotes.size)))
            log.info(quoteResponse.toString)
			sender ! quoteResponse
        }
    }

    // We'll cover the purpose of this method in the Testing section
    def quoteList = quotes
}

// 2.1 日志与测试
import akka.actor.ActorSystem
// 请参见 src/test/scala/sec20/TeacherPreTest.scala
// 请参见 src/test/scala/sec20/TeacherTest.scala

// 2.2 带参数的 Actor
class TeacherLogParameterActor (quotes:List[String]) extends Actor with ActorLogging {

  lazy val _quotes = quotes

  def receive = {

    case TeacherProtocol.QuoteRequest => {

      //get a random element (for now)
      val quoteResponse = TeacherProtocol.QuoteResponse(_quotes(Random.nextInt(_quotes.size)))
      log.info(quoteResponse.toString())

      //log.info("Quote printed") //This message is just to assert from the testcase      
    }

  }
      
  def quoteList=_quotes
}

// 3. 接收消息，得到响应

object StudentProtocol {
	case class InitSignal()
}

class StudentActor (teacherActorRef: ActorRef) extends Actor with ActorLogging {
	def receive = {
		case StudentProtocol.InitSignal => {
			teacherActorRef ! TeacherProtocol.QuoteRequest
		}

		case TeacherProtocol.QuoteResponse(quoteString) => {
			log.info("Received QuoteResponse from Teacher")
			log.info(s"Printing from Student Actor $quoteString")
		}
	}
}

// 4. 向其他 Actor 发送消息
// 配置及调度
import com.typesafe.config.ConfigFactory
// ActorSystem("a") 等同于 ActorSystem("a", ConfigFactory.load())
// 覆盖 application.conf 默认配置
// val actorSystem = ActorSystem("UniversityMessageSystem", ConfigFactory.parseString("""akka.loggers=["akka.testkit.TestEventListener"]"""))
// val actorSystem = ActorSystem("UniversityMessageSystem", ConfigFactory.load("cover-application.conf"))

import scala.concurrent.duration._  // 5 seconds 可以用
class StudentDelayedActor(teacherActorRef: ActorRef) extends Actor with ActorLogging {
    def receive = {
        case StudentProtocol.InitSignal => {
            import context.dispatcher   // 隐式参数 ExecutionContext
            //context.system.scheduler.scheduleOnce(5 seconds, teacherActorRef, TeacherProtocol.QuoteRequest)
            context.system.scheduler.schedule(0 seconds, 3 seconds, teacherActorRef, TeacherProtocol.QuoteRequest)
        }

		case TeacherProtocol.QuoteResponse(quoteString) => {
			log.info("Received QuoteResponse from Teacher")
			log.info(s"Printing from Student Actor $quoteString")
			// context.system.shutdown()
        }
    }
}


// 5. 消息通道

// 6. 同步消息和 Future

// 7. 共享线程

// 8. Actor 的生命周期

// 9. 将多个 Actor 链接在一起

// 10. Actor 的设计

object CourseTest extends App {
    println ("sec20.course.CourseTest")
    // 由于 actor 运行在多线程环境中，下面的 println 并不会按顺序打印

    // 1.
    // println("------------------------------  section 1 -------------------------");
	val system = ActorSystem("UniversityMessageSystem")
	val pingActor = system.actorOf(PingActor.props, "pingActor")
	pingActor ! PingActor.Initialize
	// This example app will ping pong 3 times and thereafter terminate the ActorSystem - 
	// see counter logic in PingActor
	// system.awaitTermination()   // 会等某一个 actor 调用 context.system.shutdown 之后结束 // terminating or terminated 就不能再创建 actor 了
    // system.shutdown()

    // 2.
    // println("------------------------------  section 2 -------------------------");
    val teacherActorRef = system.actorOf(Props[TeacherActor], "teacherActorRef")
    teacherActorRef ! TeacherProtocol.QuoteRequest

    // 3.
    // println("------------------------------  section 3 -------------------------");

	val studentRef = system.actorOf(Props(new StudentActor(teacherActorRef)), "studentRef")
	studentRef ! StudentProtocol.InitSignal

    // 4.
    // println("------------------------------  section 4 -------------------------");
    // 打印配置
    println("***** " + system.settings.config.getValue("akka.loggers"))
    // 覆盖属性
    //import akka.testkit.TestEventListener
    //val config = ConfigFactory.parseString("""akka.loggers=["akka.testkit.TestEventListener"]""")
    //val sys2 = ActorSystem("UniversityMessageSystem", config.withFallback(ConfigFactory.load()))
    //println("***** " + sys2.settings.config.getValue("akka.loggers"))

	val studentDelayedRef = system.actorOf(Props(new StudentDelayedActor(teacherActorRef)), "studentDelayedRef")
	studentDelayedRef ! StudentProtocol.InitSignal

    // 5.
    // println("------------------------------  section 5 -------------------------");

    // 6.
    // println("------------------------------  section 6 -------------------------");

    // 7.
    // println("------------------------------  section 7 -------------------------");

    // 8.
    // println("------------------------------  section 8 -------------------------");

    // 9.
    // println("------------------------------  section 9 -------------------------");

    // 10.
    // println("------------------------------  section 10 -------------------------");
    // system.awaitTermination()

    Thread.sleep(10000)
    system.shutdown()
}
