package scanning.application

import cats.syntax.all.*
import cats.Applicative
import core.domain.project.{ProjectScanConfig, ProjectScanConfigRepository}

trait ProjectService[F[_]]:
  def all: F[List[ProjectScanConfig]]
  def find(name: String): F[Option[ProjectScanConfig]]

object ProjectService:
  def make[F[_]: Applicative](repository: ProjectScanConfigRepository[F])
      : ProjectService[F] = new:
    def all: F[List[ProjectScanConfig]] = repository.all
    def find(name: String): F[Option[ProjectScanConfig]] =
      all.map: configs =>
        configs.find: config =>
          config.project.name == name
