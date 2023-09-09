package core.domain

object task:
  trait TaskProcessor[F[_]]:
    def add(task: => F[Unit]): F[Unit]
