package sec20

import scala.concurrent.duration._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.MustMatchers
import org.scalatest.WordSpecLike
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.EventFilter
import akka.testkit.ImplicitSender   // 成为一个 sender，否则 sender 是 deadLetters
import akka.testkit.TestKit
import akka.testkit.TestActorRef
import akka.testkit.TestProbe

import sec20.course.StudentActor
import sec20.course.StudentDelayedActor
import sec20.course.TeacherActor
import sec20.course.QuoteRepositoryActor
import sec20.course.StudentProtocol.InitSignal
import sec20.course.TeacherProtocol.QuoteRequest
import sec20.course.TeacherProtocol.QuoteRepositoryRequest
import sec20.course.TeacherProtocol.QuoteRepositoryResponse
import sec20.course.TeacherLogParameterActor
import sec20.course.TeacherActorWatcher

class TeacherWatchTest extends TestKit(ActorSystem("UniversityMessageSystem", ConfigFactory.parseString("""
                                    akka {
                                        loggers = ["akka.testkit.TestEventListener"]
                                        test {
                                            filter-leeway = 7s
                                        }
                                    }
                                """)))
    with WordSpecLike
    with MustMatchers
    with BeforeAndAfterAll 
    with ImplicitSender {

        "A QuoteRepositoryActor" must {
            "send back a termination message to the watcher on 4th message" in {
                val quoteRepository = TestActorRef[QuoteRepositoryActor]("quoteRepository")

                val testProbe = TestProbe()
                testProbe.watch(quoteRepository) // Let's watch the Actor

                within (1000 millis) {
                    var receivedQuotes = List[String] ()
                    (1 to 3).foreach (_ => quoteRepository ! QuoteRepositoryRequest)
                    receiveWhile() {
                        case QuoteRepositoryResponse(quoteString) => {
                            receivedQuotes = receivedQuotes :+ quoteString
                        }
                    }

                    receivedQuotes.size must be (3)
                    println(s"receiveCount ${receivedQuotes.size}")

                    // 4th message
                    quoteRepository ! QuoteRepositoryRequest
                    testProbe.expectTerminated(quoteRepository) // Expect a Terminated Message
                }
            }
        }

        "not send back a termination message on 4th message if not watched" in {
            val quoteRepository = TestActorRef[QuoteRepositoryActor]("quoteRepository")

            val testProbe = TestProbe()
            testProbe.watch(quoteRepository) // Let's watch the Actor

            within (1000 millis) {
                var receivedQuotes = List[String] ()
                (1 to 3).foreach (_ => quoteRepository ! QuoteRepositoryRequest)
                receiveWhile() {
                    case QuoteRepositoryResponse(quoteString) => {
                        receivedQuotes = receivedQuotes :+ quoteString
                    }
                }

                testProbe.unwatch(quoteRepository)  // un watching

                receivedQuotes.size must be (3)
                println(s"receiveCount ${receivedQuotes.size}")

                // 4th message
                quoteRepository ! QuoteRepositoryRequest
                testProbe.expectNoMsg() // Not Watching. No Terminated Message
            }
        }

        "end back a termination message to the watcher on 4th message to the TeacherActor" in {
            // This just subscribes to the EventFilter for messages. We have asserted all that we need against the QuoteRepositoryActor in the previous testcase
            val teacherActor = TestActorRef[TeacherActorWatcher]

            within (1000 millis) {
                (1 to 3).foreach (_ => teacherActor ! QuoteRequest) // this sends a message to the QuoteRepositoryActor

                EventFilter.error(pattern="""Child Actor .* Terminated""", occurrences = 1).intercept {
                    teacherActor ! QuoteRequest // Send the dangerous 4th message
                }
            }
        }

        override def afterAll() {
            super.afterAll()
            system.shutdown()
        }
}
