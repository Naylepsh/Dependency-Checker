package core.domain

import core.domain.registry.ProjectScanConfig

object task:
  enum Task:
    case ScanTask(config: ProjectScanConfig) extends Task

  trait TaskProcessor[F[_]]:
    def add(task: Task): F[Unit]

  trait TaskHandler[F[_]]:
    def execute(task: Task): F[Unit]
