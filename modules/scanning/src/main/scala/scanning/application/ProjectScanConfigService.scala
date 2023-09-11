package scanning.application

import java.util.UUID

import cats.Monad
import cats.syntax.all.*
import core.domain.project.{ ProjectScanConfig, ProjectScanConfigRepository }

trait ProjectScanConfigService[F[_]]:
  def all: F[List[ProjectScanConfig]]
  def find(name: String): F[Option[ProjectScanConfig]]
  def add(project: ProjectScanConfig): F[Unit]
  def setEnabled(name: String, enabled: Boolean): F[Option[ProjectScanConfig]]

object ProjectScanConfigService:
  def make[F[_]: Monad](repository: ProjectScanConfigRepository[F])
      : ProjectScanConfigService[F] = new:
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
