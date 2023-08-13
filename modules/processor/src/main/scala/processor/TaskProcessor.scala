package processor

import core.domain.task.{ Task, TaskHandler, TaskProcessor }
import cats.effect.std.{ Queue, Supervisor }
import cats.effect.{ Ref, Resource, Spawn, Temporal }
import cats.Monad
import cats.syntax.all.*
import concurrent.duration.*

object TaskProcessor:
  def make[F[_]: Monad: Temporal](
      handler: TaskHandler[F],
      maxWorkers: Int,
      stopPredicate: F[Boolean],
      mainWorkerSleepTime: Option[Duration]
  ): Resource[F, TaskProcessor[F]] =
    Supervisor[F](await = true).evalMap: supervisor =>
      for
        queue       <- Queue.unbounded[F, Task]
        workerCount <- Ref.of[F, Int](1)
        mainWorker = TaskWorker(queue, handler, mainWorkerSleepTime)
        processor = TaskProcessor.make(
          supervisor,
          queue,
          handler,
          workerCount,
          maxWorkers
        )
        _ <- supervisor.supervise(
          mainWorker.runOnce.whileM_(stopPredicate.map(!_))
        )
      yield processor

  private def make[F[_]: Monad: Temporal](
      supervisor: Supervisor[F],
      queue: Queue[F, Task],
      handler: TaskHandler[F],
      workerCount: Ref[F, Int],
      maxWorkers: Int
  ): TaskProcessor[F] = new:
    def add(task: Task): F[Unit] =
      for
        tasksOnQueue <- queue.size
        busyWorkers  <- workerCount.get
        _            <- queue.offer(task)
        _ <-
          if tasksOnQueue > 0 && busyWorkers <= maxWorkers
          then
            val execution = workerCount.update(_ + 1)
              *> TaskWorker(queue, handler).runOnce
              *> workerCount.update(_ - 1)
            supervisor.supervise(execution).void
          else Monad[F].unit
      yield ()

private class TaskWorker[F[_]: Monad: Temporal](
    queue: Queue[F, Task],
    handler: TaskHandler[F],
    sleepOnEmpty: Option[Duration] = None
):
  def runForever: F[Unit] = runOnce.foreverM

  def runOnce: F[Unit] =
    queue.tryTake.flatMap:
      case None =>
        sleepOnEmpty
          .map(Temporal[F].sleep)
          .getOrElse(Monad[F].unit)
      case Some(task) =>
        handler.execute(task)
