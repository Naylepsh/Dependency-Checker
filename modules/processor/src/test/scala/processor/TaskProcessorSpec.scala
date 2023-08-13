package processor

import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.{ IO, Ref }
import core.domain.task.TaskProcessor
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import core.domain.registry.ProjectScanConfig
import concurrent.duration.*

class TaskProcessorSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  import TaskProcessorSpec.*

  "Execute tasks on the main worker" in:
    Ref[IO].of[Int](0).flatMap: taskCount =>
      TaskProcessor.make(
        1,
        taskCount.get.map(_ > 0),
        None
      ).use: processor =>
        taskCount.get.asserting(_ shouldBe 0)
          *> processor.add(incTaskCount(taskCount))
          // Need to sleep to give processor some time to process the task
          *> IO.sleep(4.seconds)
          *> taskCount.get.asserting(_ shouldBe 1)

  "Execute tasks on the additional workers" in:
    Ref[IO].of[Int](0).flatMap: taskCount =>
      TaskProcessor.make(
        2,
        // Kill the main worker after the first task just to ensure additional worker
        taskCount.get.map(_ == 1),
        None
      ).use: processor =>
        taskCount.get.asserting(_ shouldBe 0)
          *> processor.add(slowIncTaskCount(taskCount))
          *> IO.sleep(1.second)
          *> processor.add(slowIncTaskCount(taskCount))
          // Need to sleep to give processor some time to process the task
          // 4 seconds should be enough for two workers
          // but not enough for one
          *> IO.sleep(6.seconds)
          *> taskCount.get.asserting(_ shouldBe 2)

object TaskProcessorSpec:
  def incTaskCount(taskCount: Ref[IO, Int]) =
    taskCount.update(_ + 1)

  def slowIncTaskCount(taskCount: Ref[IO, Int]) =
    IO.sleep(3.seconds) *> incTaskCount(taskCount)
