package sec20
package course
// 注意，scala.2.10 版后改为 akka 的 actor 库， 这里使用 akka actor 库
// 学习系列文章：   http://rerun.me/2014/09/19/akka-notes-actor-messaging-1/
// http://it.deepinmind.com/akka/2014/10/22/akka-notes-logging-and-testing.html
// --------------
// 执行 activator 的 about 命令可以自动 resolve 依赖库

import akka.actor.{Actor, ActorLogging, Props, ActorRef}
import akka.actor.ActorSystem

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
			if (counter == 3) { context.system.shutdown } // 调用 shutdown，会触发 system 关闭事件，后面的 schedule 任务不再执行
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
    case class QuoteRepositoryRequest()
    case class QuoteResponse(quoteString: String)
    case class QuoteRepositoryResponse(quoteString: String)
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
// 4.1 配置及调度
import com.typesafe.config.ConfigFactory
// ActorSystem("a") 等同于 ActorSystem("a", ConfigFactory.load())
// 覆盖 application.conf 默认配置
// val actorSystem = ActorSystem("UniversityMessageSystem", ConfigFactory.parseString("""akka.loggers=["akka.testkit.TestEventListener"]"""))
// val actorSystem = ActorSystem("UniversityMessageSystem", ConfigFactory.load("cover-application.conf"))

import scala.concurrent.duration._  // 5 seconds 需要 import
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

// 4.2 在消息中带有继续处理的 actor 引用，可处理后继消息

object EventProtocol {
    case class Init()
    case class Event(head: String, body: String, nextActor: ActorRef)
    case class FinalEvent(head: String, body: String)
}

class Engine extends Actor with ActorLogging {
    val normalizationActor = context.actorOf(Props[Normalization], "normalizationActor")
    val sinkActor = context.actorOf(Props[Sink], "sinkActor")

    def receive = {
        case EventProtocol.Init => {
            log.info("Init Engine")
            log.info("create events and send to Normalization")
            (1 to 3000).foreach (i => normalizationActor ! EventProtocol.Event("initHead(" + i + ")", "initBody", sinkActor))
            context.system.shutdown
        }
    }
}

class Normalization extends Actor with ActorLogging {
    def receive = {
        case EventProtocol.Event(head, body, nextActor) => {
            nextActor ! EventProtocol.FinalEvent(head + "-Normalization", body + "-Normalization")
        }
    }
}

class Sink extends Actor with ActorLogging {
    def receive = {
        case EventProtocol.FinalEvent(head, body) => {
            log.info(s"Save the Event [$head]-[$body]")
        }
    }
}

// 5. 消息通道
// 似乎在 akka 中没有 Channel 概念了，应该是用 EventBus 来处理消息总线
// 可参考 http://www.tuicool.com/articles/NzMb6vr
// 参考后发现 EventBus 是处理消息订阅和发布的，与原本的 channel 不同
// 似乎用 AtLeastOnceDelivery 替换原来的 channel，但为何是 persistence 包中？仅与持久化相关吗？

// 6. 同步消息和 Future
// 参考 akka.dispatch.Futures

// 7. 共享线程
// 好像 akka 中没有 react 方法了，akka 本身就是异步事件驱动的，不是通过 while 循环来处理 receive 方法的
// 从日志中也可以看出来，akka 就是共享线程的

// 8. Actor 的生命周期
// --------------
// 通过构造函数创建 Actor
// 调用它的 preStart 方法
// 接收消息时，调用他的 receive 方法
// 最后调用 postStop 方法将 Actor 置为终结状态
import akka.event.LoggingReceive
class BasicLifecycleLoggingActor extends Actor with ActorLogging {
    // 构造方法中的过程只会执行一次，Restart 后也会执行构造方法
    log.info("Inside BasicLifecycleLoggingActor Constructor")
    log.info(context.self.toString)

    override def preStart() = {
        // preStart 方法中的过程会在重启时重新执行
        log.info("Inside the preStart method of BasicLifecycleLoggingActor")
    }

    def receive = LoggingReceive {
        case "hello" => log.info ("hello")
        // 通过消息来终止 actor
        case "stop" => context.stop(self)
    }

    override def postStop() = {
        log.info("Inside postStop method of BasicLifecycleLoggingActor")
    }
}

// 9. 将多个 Actor 链接在一起
// 参考 Actor 路径
// 参考 akka 监控与监督，子 actor 失败后，父 actor 可以对下一步情况做控制

// 创建一个子 actor
class TeacherSupervisor extends Actor with ActorLogging {
    val teacherActor = context.actorOf(Props[TeacherActor], "teacherActor")
    def receive = {
		case "hello" => {}
    }
}

// 任何一个 actor 都可以对 actorSystem 中的其他 actor 进行监控，但监督则对 actor 的父子关系有约束，监督实际上指的是对子 actor 中出现的异常情况的处理
import akka.actor.actorRef2Scala
import akka.actor.PoisonPill
import akka.actor.Terminated

class QuoteRepositoryActor extends Actor with ActorLogging {
    val quotes = List(
        "1. Moderation is for cowards",
        "2. Anything worth doing is worth overdoing",
        "3. The trouble is you think you have time",
        "4. You never gonna know if you never even try"
    )

    var repoRequestCount: Int = 1

    def receive = {
        case TeacherProtocol.QuoteRepositoryRequest => {
            if (repoRequestCount > 3) {
                self ! PoisonPill
            } else {
                // Get a random Quote from the list and construct a response
                val quoteResponse = TeacherProtocol.QuoteRepositoryResponse(quotes(Random.nextInt(quotes.size)))

                log.info(s"QuoteRequest received in QuoteRepositoryActor. Sending response to Sender $quoteResponse")
                log.info(s"Sender Actor is: $sender")
                repoRequestCount = repoRequestCount + 1
                sender ! quoteResponse
            }
        }
    }
}

class TeacherActorWatcher extends Actor with ActorLogging {
    val quoteRepositoryActor = context.actorOf(Props[QuoteRepositoryActor], "quoteRepositoryActor")
    context.watch(quoteRepositoryActor)

    def receive = {
        case TeacherProtocol.QuoteRequest => {
            quoteRepositoryActor ! TeacherProtocol.QuoteRepositoryRequest
        }
        case Terminated(terminatedActorRef) => {
            log.error(s"Child Actor {$terminatedActorRef} Terminated")
        }
    }
}

// 监督策略
// AllForOneStrategy    // 一个分支失败，其他的也一起失败
// OneForOneStrategy    // 一个分支失败，其他的仍然可以继续
/*
// DefaultSupervisorStrategy
    final val defaultDecider: Decider = {
        case _: ActorInitializationException => Stop
        case _: ActorKilledException         => Stop
        case _: DeathPactException           => Stop    // 观察到 stop，但没有处理 Terminated 事件的 actor 会得到一个 DeathPactException
        case _: Exception                    => Restart
    }
    final val defaultStrategy: SupervisorStrategy = {
        OneForOneStrategy()(defaultDecider)
    }
*/
// StoppingSupervisorStrategy

// 指令
// Stop, Resume, Escalate, Restart
// Stop:        The child actor is stopped in case of exception and any messages to the stopped actor would obviously go to the deadLetters queue.
// Resume:      The child actor just ignores the message that threw the exception and proceeds with processing the rest of the messages in the queue. Resume 仅抛出异常，会调用 preStart 吗？
// Restart:     The child actor is stopped and a brand new actor is initialized. Processing of the rest of the messages in the mailbox continue. The rest of the world is unaware that this happened since the same ActorRef is attached to the new Actor.
// Escalate:    The supervisor ducks the failure and lets its supervisor handle the exception.

import akka.actor.AllForOneStrategy
import akka.actor.OneForOneStrategy
import akka.actor.Kill
import akka.actor.SupervisorStrategy.Escalate
import akka.actor.SupervisorStrategy.Restart
import akka.actor.SupervisorStrategy.Resume
import akka.actor.SupervisorStrategy.Stop

@SerialVersionUID(1L)
class MinorRecoverableException(message: String, cause: Throwable) extends Exception(message, cause) with Serializable {
    def this(msg: String) = this(msg, null)
}

@SerialVersionUID(1L)
class MajorUnRecoverableException(message: String, cause: Throwable) extends Exception(message, cause) with Serializable {
    def this(msg: String) = this(msg, null)
}

class TeacherActorOneForOne extends Actor with ActorLogging {
    def receive = {
        case TeacherProtocol.QuoteRequest => {}
    }

    override def supervisorStrategy = OneForOneStrategy() {
        case _: MinorRecoverableException => Restart
        case _: Exception => Stop
    }
}

class TeacherActorAllForOne extends Actor with ActorLogging {
    def receive = {
        case TeacherProtocol.QuoteRequest => {}
    }

    override def supervisorStrategy = AllForOneStrategy() {
        case _: MajorUnRecoverableException => Stop
        case _: Exception => Escalate
    }
}

class DeathPactExceptionParentActor extends Actor with ActorLogging {
    def receive = {
        case "create_child" => {
            log.info ("creating child")
            val child = context.actorOf(Props[DeathPactExceptionChildActor])
            context.watch(child)    // Watches but doesn't handle terminated message. Throwing DeathPactException here.
            child ! "stop"
        }
        case "someMessage" => log.info ("some message")
        // Doesn't handle terminated message
        // case Terminated(_) =>
    }
}

class DeathPactExceptionChildActor extends Actor with ActorLogging {
    def receive = {
        case "stop" => {
            log.info ("Actor going to stop and announce that it's terminated")
            self ! PoisonPill
        }
    }
}

class OtherExceptionParentActor extends Actor with ActorLogging {
	def receive = {
		case "create_child" => {
			log.info("creating child")
			val child = context.actorOf(Props[OtherExceptionChildActor])

			child ! "throwSomeException"
			child ! "someMessage"
		}
	}

	// 在此处声明的监督是对子 actor 抛出的异常进行处理
	// Resume 的话，仅打印一条 warning 信息，其他什么也不做，也不会 restart
	// 子 actor 的 preStart 方法也不会被调用
	// Restart 的话，会重新构造子 actor，会调用 preStart
    override def supervisorStrategy = OneForOneStrategy() {
        case _: Exception => Restart // Resume
    }
}

class OtherExceptionChildActor extends Actor with ActorLogging {
	log.info("In OtherExceptionChildActor constructor")

	override def preStart() = {
		log.info ("Starting child actor")
	}

	def receive = {
		case "throwSomeException" => {
			throw new Exception ("I'm getting thrown for no reason")
		}
		case "someMessage" => log.info ("Restarted and printing some Message")
	}

	override def postStop() = {
		log.info ("Stopping child actor")
	}

	override def postRestart(reason: Throwable): Unit = {	// 何时调用？
		preStart()
	}
}

class EscalateExceptionTopLevelActor extends Actor with ActorLogging {
	override val supervisorStrategy = OneForOneStrategy() {
		case _: Exception => {
			log.info("The exception from the Child is now handled by the Top level Actor. Stopping Parent Actor and its children.")
			Stop	// Stop will stop the Actor that threw this Exception and all its children
		}
	}

	def receive = {
		case "create_parent" => {
			log.info("creating parent")
			val parent = context.actorOf(Props[EscalateExceptionParentActor], "parentActor")
			parent ! "create_child" // Sending message to next level
		}
		case _ =>
	}
}

class EscalateExceptionParentActor extends Actor with ActorLogging {
	override def preStart = {
		log.info ("Parent Actor started")
	}

	override val supervisorStrategy = OneForOneStrategy() {
		case _: Exception => {
			log.info("The exception is ducked by the Parent Actor. Escalating to TopLevel Actor")
			Escalate
		}
	}

	def receive = {
		case "create_child" => {
			log.info ("creating child")
			val child = context.actorOf(Props[EscalateExceptionChildActor], "childActor")
			child ! "throwSomeException"
		}
		case _ =>
	}

	override def postStop() = {
		log.info("Stopping parent Actor")
	}
}

class EscalateExceptionChildActor extends Actor with ActorLogging {
	override def preStart = {
		log.info ("Child Actor started")
	}

	def receive = {
		case "throwSomeException" => {
			throw new Exception("I'm getting thrown for no reason.")
		}
		case _ =>
	}

	override def postStop() = {
		log.info("Stopping child Actor")
	}
}

// 10. Actor 的设计
// 1. 避免使用共享状态，actor 不要访问外部数据，可根据消息修改内部数据
// 2. 不要调用 actor 的方法，而应该发送消息，如果 actor 中没有其他 actor 的引用，就容易避免
//      这时，应该会使用到 EventBus
// 3. 保持每个 actor 的简单性，接收任务做计算，把结果传递下去，而不是改变状态
// 4. 消息中包含上下文数据，actor 就可理解消息，而不需要访问别的对象
// 5. 不需要复杂的 reply，应该组成一个 actor 网络，每个 actor 做部分计算，有些 actor 合并最终结果
// 6. 小心消息阻塞
// 7. 监控和监督 actor 执行，对失败的 actor 做正确的处理：登记问题或者重试，或者简单抛出问题

object CourseTest extends App {
    println ("sec20.course.CourseTest")
    // 由于 actor 运行在多线程环境中，下面的 println 并不会按顺序打印

    // 1.
    // println("------------------------------  section 1 -------------------------");
    // see PingPongApp

    // 2.
    // println("------------------------------  section 2 -------------------------");
	val system = ActorSystem("UniversityMessageSystem")
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

    // see EventApp

    // 5.
    // println("------------------------------  section 5 -------------------------");

    // 6.
    // println("------------------------------  section 6 -------------------------");

    // 7.
    // println("------------------------------  section 7 -------------------------");

    // 8.
    // println("------------------------------  section 8 -------------------------");
    // see LifecycleApp

    // 9.
    // println("------------------------------  section 9 -------------------------");
    // see TeacherWatchTest
    // see ActorInitializationExceptionApp
    // see ActorKilledExceptionApp
    // see DeathPactExceptionApp
	// see OtherExceptionApp
	// see EscalateExceptionApp

    // 10.
    // println("------------------------------  section 10 -------------------------");
    // system.awaitTermination()

    Thread.sleep(10000)
    system.shutdown()
}

object PingPongApp extends App {
	val system = ActorSystem("UniversityMessageSystem")
	val pingActor = system.actorOf(PingActor.props, "pingActor")
	pingActor ! PingActor.Initialize

	// This example app will ping pong 3 times and thereafter terminate the ActorSystem - 
	// see counter logic in PingActor

	system.awaitTermination()   // 会等某一个 actor 调用 context.system.shutdown 之后结束 // terminating or terminated 就不能再创建 actor 了
    system.shutdown()
}

object EventApp extends App {
    val actorSystem = ActorSystem("EventSystem")
    val engine = actorSystem.actorOf(Props[Engine], "engineActor")
    engine ! EventProtocol.Init

	actorSystem.awaitTermination()   // 会等某一个 actor 调用 context.system.shutdown 之后结束 // terminating or terminated 就不能再创建 actor 了
    actorSystem.shutdown()
}

import akka.actor.DeadLetter
object LifecycleApp extends App {
    val actorSystem = ActorSystem("LefecycleActorSystem")
    val lifecycleActor = actorSystem.actorOf(Props[BasicLifecycleLoggingActor], "lifecycleActor")
    val deadLetterListener = actorSystem.actorOf(Props(new Actor{
            def receive = {
                case deadLetter: DeadLetter => println(s"FROM CUSTOM LISTENER $deadLetter")
            }
        }))
    actorSystem.eventStream.subscribe(deadLetterListener, classOf[DeadLetter]) // 额外订阅一下 deadletter

	lifecycleActor ! "hello"
	lifecycleActor ! "bubble"


    // 可以通过 stop 让 actor 终止，可能会收到一个 deadletter
    // actorSystem.stop(lifecycleActor)

    // 可以 kill actor
    // lifecycleActor ! Kill
    
    // 通过发消息来终止 actor
	// lifecycleActor ! "stop"
    
    // 通过发 PoisonPill 来终止 actor
	lifecycleActor ! PoisonPill
	lifecycleActor ! "hello"

    Thread.sleep(2000)      // 等一会，以便可以处理到 deadletter
    actorSystem.shutdown()
}

object ActorPathApp extends App {
    val actorSystem = ActorSystem("SupervisionActorSystem")
    //val lifecycleActor = actorSystem.actorOf(Props[BasicLifecycleLoggingActor]) // akka://SupervisionActorSystem/user/$a
    val lifecycleActor = actorSystem.actorOf(Props[BasicLifecycleLoggingActor], "lifecycleActor")
    println (lifecycleActor.path)
    actorSystem.shutdown()
}

object ActorInitializationExceptionApp extends App {
    val actorSystem = ActorSystem("ActorInitializationException")
    val actor = actorSystem.actorOf(Props(new Actor with ActorLogging() {
            override def preStart = {
                throw new Exception("Some random exception")
            }

            def receive = {
                case _ =>
            }
        }), "initializationExceptionActor")
    actor ! "someMessageThatWillGoToDeadLetter"
    Thread.sleep(2000)      // 等一会，以便可以处理到 deadletter
    actorSystem.shutdown()
}

object ActorKilledExceptionApp extends App {
    val actorSystem = ActorSystem("ActorKilledExceptionSystem")
    val actor = actorSystem.actorOf(Props(new Actor with ActorLogging() {
            def receive = {
                case message:String => log.info(message)
                case _ =>
            }
        }), "actorKilledExceptionActor")

    actor ! "something"
    actor ! Kill
    actor ! "something else that falls into dead letter queue"
    Thread.sleep(2000)      // 等一会，以便可以处理到 deadletter
    actorSystem.shutdown()
}

object DeathPactExceptionApp extends App {
    val actorSystem = ActorSystem("DeathPactExceptionSystem")
    val actor = actorSystem.actorOf(Props[DeathPactExceptionParentActor])
    actor ! "create_child" // throws DeathPactException

    Thread.sleep(2000)      // 等一会
    actor ! "someMessage"   // Message goes to DeadLetters
    actorSystem.shutdown()
}

object OtherExceptionApp extends App {
    val actorSystem = ActorSystem("OtherExceptionSystem")
    val actor = actorSystem.actorOf(Props[OtherExceptionParentActor])
    actor ! "create_child"

    Thread.sleep(2000)      // 等一会
    actorSystem.shutdown()
}

object EscalateExceptionApp extends App {
    val actorSystem = ActorSystem("EscalateExceptionSystem")
    val actor = actorSystem.actorOf(Props[EscalateExceptionTopLevelActor], "topLevelActor")
    actor ! "create_parent"

    Thread.sleep(2000)      // 等一会
    actorSystem.shutdown()
}
