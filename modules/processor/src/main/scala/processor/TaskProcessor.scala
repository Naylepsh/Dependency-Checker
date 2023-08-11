package processor

import core.domain.task.{ Task, TaskHandler, TaskProcessor }
import cats.effect.std.{ Queue, Supervisor }
import cats.effect.{ Ref, Spawn, Temporal }
import cats.Monad
import cats.syntax.all.*
import concurrent.duration.*

object TaskProcessor:
  def make[F[_]: Monad: Temporal](
      queue: Ref[F, Queue[F, Task]],
      handler: TaskHandler[F],
      maxWorkers: Int
  ): F[TaskProcessor[F]] =
    Supervisor[F](await = true).use: supervisor =>
      Ref.of[F, Int](1).flatMap: workerCount =>
        val mainWorker = TaskWorker(queue, handler, 5.seconds.some)

        mainWorker.runForever.map: _ =>
          TaskProcessor.make(
            supervisor,
            queue,
            handler,
            workerCount,
            maxWorkers
          )

  private def make[F[_]: Monad: Temporal](
      supervisor: Supervisor[F],
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
            val execution = workerCount.update(_ + 1)
              *> TaskWorker(queue, handler).runOnce
              *> workerCount.update(_ - 1)
            supervisor.supervise(execution).void
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
