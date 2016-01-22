package sec20

import org.scalatest.BeforeAndAfterAll
import org.scalatest.MustMatchers
import org.scalatest.WordSpecLike
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.EventFilter
import akka.testkit.TestKit
import akka.testkit.TestActorRef

import sec20.course.StudentActor
import sec20.course.TeacherActor
import sec20.course.StudentProtocol.InitSignal
import sec20.course.TeacherProtocol.QuoteRequest
import sec20.course.TeacherLogParameterActor

class TeacherTest extends TestKit(ActorSystem("UniversityMessageSystem", ConfigFactory.parseString("""akka.loggers = ["akka.testkit.TestEventListener"]""")))
    with WordSpecLike
    with MustMatchers
    with BeforeAndAfterAll {

        // 4. Verifying log messages from eventStream
		"be verifiable via EventFilter in response to a QuoteRequest that is sent" in {
			val teacherRef = TestActorRef[TeacherActor]
			EventFilter.info(pattern="QuoteResponse*", occurrences = 1) intercept {
				teacherRef ! QuoteRequest
			}
		}

		// 5. have a quote list of the same size as the input parameter

		"have a quote list of the same size as the input parameter" in {
			val quotes = List(
				"Moderation is for cowards",
				"Anything worth doing is worth overdoing",
				"The trouble is you think you have time",
				"You never gonna know if you never even try"
			)

			val teacherRef = TestActorRef(new TeacherLogParameterActor(quotes))

			teacherRef.underlyingActor.quoteList must have size (4)
			EventFilter.info(pattern = "QuoteResponse*", occurrences = 1) intercept {
				teacherRef ! QuoteRequest
			}
		}

		"A student" must {
			"log a QuoteResponse eventually when an InitSignal is sent to it" in {
				val teacherRef = TestActorRef[TeacherActor]
				val studentRef = TestActorRef(new StudentActor(teacherRef))

				EventFilter.info(start = "Printing from Student Actor", occurrences = 1) intercept {
					studentRef ! InitSignal
				}
			}
		}

        override def afterAll() {
            super.afterAll()
            system.shutdown()
        }
}
