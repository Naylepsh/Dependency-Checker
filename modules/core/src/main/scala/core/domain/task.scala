package core.domain

import core.domain.registry.ProjectScanConfig

object task:
  trait TaskProcessor[F[_]]:
    def add(task: => F[Unit]): F[Unit]
