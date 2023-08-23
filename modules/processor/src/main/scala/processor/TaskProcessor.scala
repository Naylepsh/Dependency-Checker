package processor

import core.domain.task.TaskProcessor
import cats.effect.std.{ Queue, Supervisor }
import cats.effect.{ Ref, Resource, Spawn, Temporal }
import cats.Monad
import cats.syntax.all.*
import concurrent.duration.*
import org.legogroup.woof.{ *, given }

private type Task[F[_]] = () => F[Unit]

object TaskProcessor:

  def make[F[_]: Monad: Temporal: Logger](
      maxWorkers: Int,
      stopPredicate: F[Boolean],
      mainWorkerSleepTime: Option[Duration]
  ): Resource[F, TaskProcessor[F]] =
    Supervisor[F](await = true).evalMap: supervisor =>
      for
        queue       <- Queue.unbounded[F, Task[F]]
        workerCount <- Ref.of[F, Int](1)
        mainWorker = TaskWorker(queue)
        processor = TaskProcessor.make(
          supervisor,
          queue,
          workerCount,
          maxWorkers
        )
        _ <- supervisor.supervise(
          mainWorker.runOnce(mainWorkerSleepTime).whileM_(stopPredicate.map(!_))
        )
      yield processor

  private def make[F[_]: Monad: Temporal: Logger](
      supervisor: Supervisor[F],
      queue: Queue[F, Task[F]],
      workerCount: Ref[F, Int],
      maxWorkers: Int
  ): TaskProcessor[F] = new:
    def add(task: => F[Unit]): F[Unit] =
      for
        _            <- Logger[F].debug("Adding a task on the queue")
        _            <- queue.offer(() => task)
        tasksOnQueue <- queue.size
        busyWorkers  <- workerCount.get
        _ <-
          if 0 < tasksOnQueue && busyWorkers < maxWorkers
          then
            val worker = TaskWorker(queue)
            Logger[F].debug(s"Adding a new worker (to existing $busyWorkers)")
              *> workerCount.update(_ + 1)
              *> supervisor.supervise(
                worker.runWhileQueueIsNotEmpty
                  *> workerCount.update(_ - 1)
                  *> Logger[F].debug("Killing the worker")
              ).void
          else
            Logger[F].debug(
              s"Keeping the worker count the same (at $busyWorkers)"
            )
      yield ()

private class TaskWorker[F[_]: Monad: Temporal](queue: Queue[F, Task[F]]):
  def runOnce(sleepOnEmpty: Option[Duration]): F[Unit] =
    queue.tryTake.flatMap:
      case None =>
        sleepOnEmpty
          .map(Temporal[F].sleep)
          .getOrElse(Monad[F].unit)
      case Some(task) =>
        task()

  def runWhileQueueIsNotEmpty: F[Unit] =
    queue.tryTake.flatMap:
      case None       => Monad[F].unit
      case Some(task) => task() *> runWhileQueueIsNotEmpty
