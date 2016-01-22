package sec20

import org.scalatest.BeforeAndAfterAll
import org.scalatest.MustMatchers
import org.scalatest.WordSpecLike
import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestActorRef
import sec20.course.TeacherActor
import sec20.course.TeacherProtocol.QuoteRequest
class TeacherPreTest extends TestKit(ActorSystem("UniversityMessageSystem"))
    with WordSpecLike
    with MustMatchers
    with BeforeAndAfterAll {

        // 1. Sends message to the Print Actor. Not even a testcase actually
        "A teacher" must {
            "print a quote when a QuoteRequest message is sent" in {
                val teacherRef = TestActorRef[TeacherActor]
                teacherRef ! QuoteRequest
            }
        }

        // 2. Sends message to the Log Actor. Again, not a testcase per se
        "A teacher with ActorLogging" must {
            "log a quote when a QuoteRequest message is sent" in {
                val teacherRef = TestActorRef[TeacherActor]
                teacherRef ! QuoteRequest
            }
        }

		// 3. Asserts the internal State of the Log Actor.
		"have a quote list of size 4" in {
			val teacherRef = TestActorRef[TeacherActor]
			teacherRef.underlyingActor.quoteList must have size (4)
			teacherRef.underlyingActor.quoteList must have size (4)
		}

        override def afterAll() {
            super.afterAll()
            system.shutdown()
        }
}
