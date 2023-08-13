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
        queue          <- Queue.unbounded[F, Task]
        tasksInProgess <- Ref.of[F, Int](0)
        mainWorker =
          TaskWorker(queue, handler, tasksInProgess, mainWorkerSleepTime)
        processor = TaskProcessor.make(
          supervisor,
          queue,
          handler,
          tasksInProgess,
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
      tasksInProgress: Ref[F, Int],
      maxWorkers: Int
  ): TaskProcessor[F] = new:
    def add(task: Task): F[Unit] =
      for
        _               <- queue.offer(task)
        inProgressCount <- tasksInProgress.get
        _ <-
          /**
           * TODO: There's an edge case here:
           *  If a user adds the jobs quickly: 
           *  `processor.add(task1) *> processor.add(task2) *> processor.add(task3)`
           *  then there's a chance that inProgressCount will still be 0 
           *  by the time of `add(task2)` and `add(task3)`
           *  preventing the additional workers from spawning
           */
          if 0 < inProgressCount && inProgressCount <= maxWorkers
          then
            val worker = TaskWorker(queue, handler, tasksInProgress)
            supervisor.supervise(worker.runOnce).void
          else Monad[F].unit
      yield ()

private class TaskWorker[F[_]: Monad: Temporal](
    queue: Queue[F, Task],
    handler: TaskHandler[F],
    tasksInProgress: Ref[F, Int],
    sleepOnEmpty: Option[Duration] = None
):
  def runOnce: F[Unit] =
    queue.tryTake.flatMap:
      case None =>
        sleepOnEmpty
          .map(Temporal[F].sleep)
          .getOrElse(Monad[F].unit)
      case Some(task) =>
        tasksInProgress.update(_ + 1)
          *> handler.execute(task)
          *> tasksInProgress.update(_ - 1)
