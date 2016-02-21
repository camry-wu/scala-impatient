package sec20
// Actor

import akka.actor.{Actor, ActorLogging, Props, ActorRef, ActorSystem}
import scala.collection._

package object utils {
    def sleepNono(period: Long) {
        val beg = System.nanoTime
        var end = beg
        while (end - beg <= period) {
            end = System.nanoTime
        }
    }
}

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
    // 4 核的情况下，数量是 4, 8, 还是 100 似乎差别不大
    val WORKER_NUM = 100
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
            val y = Random.nextInt(x)
			sum += y
            utils.sleepNono(40000)      // 控制每个 cpu 每秒可处理 25000 条
            //log.info(s"$y")
		}
        case AvgActor.Eof => { 
			sender ! AvgActor.Sum(sum)
		}
    }
}

// 2.
import java.awt.Rectangle
import java.awt.image.{BufferedImage, Raster}
import java.io.File
import javax.imageio.ImageIO
class ImgActor extends Actor with ActorLogging {
    var inImage: BufferedImage = null
    var outImage: BufferedImage = null

	private var signal: Int = ImgActor.WORKER_NUM
	val workerActors = init()

    def receive = {
        case ImgActor.Init => {
            inImage = ImageIO.read(new File("src/main/scala/sec20/prac2.jpg"))

            val height = inImage.getHeight
            val width = inImage.getWidth
            val x = inImage.getMinX
            var y = inImage.getMinY
            log.info(s"Height: $height, Width: $width, x.point: $x, y.point: $y")

            outImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

            val allocateHeight = ImgActor.allocate(height)

            for (i <- 0 to (ImgActor.WORKER_NUM - 1)) {
                try {
                    workerActors(i) ! ImgActor.PartRaster(inImage.getData(new Rectangle(x, y, width, allocateHeight(i))))
                    y = y + allocateHeight(i)
                } catch {
                    case x: Exception => { x.printStackTrace() }
                }
            }
        }
        case ImgActor.PartRaster(part) => {
            log.info("complete part...")
            outImage.setData(part)
			signal -= 1
			if (signal <= 0) {
				// finish
                // write to outImage
                log.info("FINISH!")
                ImageIO.write(outImage, "jpeg", new File("src/main/scala/sec20/prac2.reverse.jpg")) 
				context.system.shutdown
			}
        }
    }

    private def init(): mutable.ArrayBuffer[ActorRef] = {
        val result = mutable.ArrayBuffer[ActorRef]()
        for (i <- 1 to ImgActor.WORKER_NUM) result += context.actorOf(Props[ImgWorkerActor], "imgWorkerActor" + i)
        result
    }
}

object ImgActor {
    val WORKER_NUM = 4
    val props = Props[ImgActor]
    case object Init
    case class PartRaster(part: Raster)

    def allocate(n: Int): mutable.ArrayBuffer[Int] = {
        val result = mutable.ArrayBuffer[Int]()
        val low = n / WORKER_NUM
        val high = low + 1
        val remainder = n % WORKER_NUM

        for (i <- 0 to remainder - 1) result += high
        for (i <- remainder to WORKER_NUM - 1) result += low

        result
    }
}

class ImgWorkerActor extends Actor with ActorLogging {
    def receive = {
        case ImgActor.PartRaster(part) => {
            log.info("receive part...")
            // 本应将收到的矩形区域反色，这里直接输出
            sender ! ImgActor.PartRaster(part)
        }
    }
}

// 3.
// 3, 4, 5, 7, 9 的习题内容相靠，一起设计处理

// 遍历给定的目录，将文件交给一个 actor 处理
class FolderVisitActor extends Actor with ActorLogging {
    def receive = {
        case _ => {}
    }
}

// 处理文件，将结果报告给结果处理器
class FileHandlerActor extends Actor with ActorLogging {
    def receive = {
        case _ => {}
    }
}

// 结果处理器，汇总结果并打印显示
class GrepResultActor extends Actor with ActorLogging {
    def receive = {
        case _ => {}
    }
}

// 4.

// 5.

// 6.
// akka actor 不是用循环处理的，此题不做

// 7.

// 8.
// 死锁演示

// 9.
// 一个有问题的 handler actor

// 10.

object PracTest1 extends App {
    // 1.
    println("------------------------------  practice 1 -------------------------");
	val begin = System.currentTimeMillis
    val actorSystem = ActorSystem("AvgSystem")
    val actor = actorSystem.actorOf(AvgActor.props)

	val beginRun = System.currentTimeMillis
	for (i <- 1 to 200000) actor ! i
	actor ! AvgActor.Eof

    actorSystem.awaitTermination()

	val endRun = System.currentTimeMillis

    actorSystem.shutdown()
	val end = System.currentTimeMillis
	println("init  time: " + (beginRun - begin))
	println("run   time: " + (endRun - beginRun))
	println("close time: " + (end - endRun))
	println("full  time: " + (end - begin))
}

object PracTest2 extends App {
    // 2.
    println("------------------------------  practice 2 -------------------------");
	val begin = System.currentTimeMillis
    val actorSystem = ActorSystem("ImgSystem")
    val actor = actorSystem.actorOf(ImgActor.props)

	val beginRun = System.currentTimeMillis
	actor ! ImgActor.Init

    actorSystem.awaitTermination()

	val endRun = System.currentTimeMillis

    actorSystem.shutdown()
	val end = System.currentTimeMillis
	println("init  time: " + (beginRun - begin))
	println("run   time: " + (endRun - beginRun))
	println("close time: " + (end - endRun))
	println("full  time: " + (end - begin))
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
