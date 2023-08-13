package processor

import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.{ IO, Ref }
import core.domain.task.{ Task, TaskHandler, TaskProcessor }
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import core.domain.registry.ProjectScanConfig
import concurrent.duration.*

class TaskProcessorSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  import TaskProcessorSpec.*

  "Execute tasks on the main worker" in:
    TestTaskHandler.make.flatMap: taskHandler =>
      TaskProcessor.make(
        taskHandler,
        1,
        taskHandler.tasksExecuted.get.map(_ > 0),
        None
      ).use: processor =>
        taskHandler.tasksExecuted.get.asserting(_ shouldBe 0)
          *> processor.add(task)
          // Need to sleep to give processor some time to process the task
          *> IO.sleep(4.seconds)
          *> taskHandler.tasksExecuted.get.asserting(_ shouldBe 1)

  "Execute tasks on the additional workers" in:
    SlowTestTaskHandler.make.flatMap: taskHandler =>
      TaskProcessor.make(
        taskHandler,
        2,
        // Kill the main worker after the first task just to ensure additional worker
        taskHandler.tasksExecuted.get.map(_ == 1),
        None
      ).use: processor =>
        taskHandler.tasksExecuted.get.asserting(_ shouldBe 0)
          *> processor.add(task)
          *> IO.sleep(1.second)
          *> processor.add(task)
          // Need to sleep to give processor some time to process the task
          // 4 seconds should be enough for two workers
          // but not enough for one
          *> IO.sleep(6.seconds)
          *> taskHandler.tasksExecuted.get.asserting(_ shouldBe 2)

object TaskProcessorSpec:
  class TestTaskHandler(val tasksExecuted: Ref[IO, Int])
      extends TaskHandler[IO]:
    def execute(task: Task): IO[Unit] = tasksExecuted.update(_ + 1)
  object TestTaskHandler:
    def make: IO[TestTaskHandler] = Ref.of[IO, Int](0).map: tasksExecuted =>
      new TestTaskHandler(tasksExecuted)

  class SlowTestTaskHandler(tasksExecuted: Ref[IO, Int])
      extends TestTaskHandler(tasksExecuted):
    override def execute(task: Task): IO[Unit] =
      IO.sleep(3.seconds) *> super.execute(task)
  object SlowTestTaskHandler:
    def make: IO[TestTaskHandler] = Ref.of[IO, Int](0).map: tasksExecuted =>
      new SlowTestTaskHandler(tasksExecuted)

  val task = Task.ScanTask(ProjectScanConfig(
    "123",
    "my-test-project",
    List.empty,
    false,
    "master"
  ))
