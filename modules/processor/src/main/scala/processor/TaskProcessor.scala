package processor

import core.domain.task.{ Task, TaskHandler, TaskProcessor }
import cats.effect.std.Queue
import cats.effect.{ Ref, Temporal }
import cats.Monad
import cats.syntax.all.*
import concurrent.duration.*

object TaskProcessor:
  def make[F[_]: Monad: Temporal](
      queue: Ref[F, Queue[F, Task]],
      handler: TaskHandler[F],
      maxWorkers: Int
  ): F[TaskProcessor[F]] =
    Ref.of[F, Int](1).flatMap: workerCount =>
      val mainWorker = TaskWorker(queue, handler, 5.seconds.some)

      mainWorker.runForever.map: _ =>
        TaskProcessor.make(queue, handler, workerCount, maxWorkers)

  private def make[F[_]: Monad: Temporal](
      queue: Ref[F, Queue[F, Task]],
      handler: TaskHandler[F],
      workerCount: Ref[F, Int],
      maxWorkers: Int
  ): TaskProcessor[F] = new:
    def add(task: Task): F[Unit] = queue.get.flatMap: q =>
      for
        tasksOnQueue <- q.size
        busyWorkers  <- workerCount.get
        _            <- q.offer(task)
        _ <-
          if tasksOnQueue > 0 && busyWorkers <= maxWorkers
          then
            workerCount.update(_ + 1)
              *> TaskWorker(queue, handler).runOnce
              *> workerCount.update(_ - 1)
          else Monad[F].unit
      yield ()

private class TaskWorker[F[_]: Monad: Temporal](
    queue: Ref[F, Queue[F, Task]],
    handler: TaskHandler[F],
    sleepOnEmpty: Option[Duration] = None
):
  def runForever: F[Unit] = runOnce.foreverM

  def runOnce: F[Unit] =
    queue.get.flatMap: q =>
      q.tryTake.flatMap:
        case None =>
          sleepOnEmpty
            .map(Temporal[F].sleep)
            .getOrElse(Monad[F].unit)
        case Some(task) => handler.execute(task)
