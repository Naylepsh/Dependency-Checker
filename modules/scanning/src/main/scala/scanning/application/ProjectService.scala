package scanning.application

import cats.syntax.all.*
import cats.Monad
import core.domain.project.{ ProjectScanConfig, ProjectScanConfigRepository }
import java.util.UUID

trait ProjectService[F[_]]:
  def all: F[List[ProjectScanConfig]]
  def find(name: String): F[Option[ProjectScanConfig]]
  def add(project: ProjectScanConfig): F[Unit]
  def setEnabled(name: String, enabled: Boolean): F[Option[ProjectScanConfig]]

object ProjectService:
  def make[F[_]: Monad](repository: ProjectScanConfigRepository[F])
      : ProjectService[F] = new:
    def all: F[List[ProjectScanConfig]] = repository.all
    def find(name: String): F[Option[ProjectScanConfig]] =
      all.map: configs =>
        configs.find: config =>
          config.project.name == name
    def add(project: ProjectScanConfig): F[Unit] = repository.save(project).void
    def setEnabled(
        name: String,
        enabled: Boolean
    ): F[Option[ProjectScanConfig]] =
      repository.setEnabled(name, enabled) *> find(name)
