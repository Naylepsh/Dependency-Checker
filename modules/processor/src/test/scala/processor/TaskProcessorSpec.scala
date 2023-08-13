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

object TaskProcessorSpec:
  class TestTaskHandler(val tasksExecuted: Ref[IO, Int])
      extends TaskHandler[IO]:
    def execute(task: Task): IO[Unit] = tasksExecuted.update(_ + 1)
  object TestTaskHandler:
    def make: IO[TestTaskHandler] = Ref.of[IO, Int](0).map: tasksExecuted =>
      new TestTaskHandler(tasksExecuted)

  val task = Task.ScanTask(ProjectScanConfig(
    "123",
    "my-test-project",
    List.empty,
    false,
    "master"
  ))
