package update.services

import java.util.UUID

import cats.Monad
import cats.syntax.all.*
import core.domain.task.TaskProcessor
import core.domain.update.{ DependencyToUpdate, UpdateDependency }
import org.legogroup.woof.{ *, given }
import update.domain.*

object UpdateGateway:
  def make[F[_]: Monad: Logger](
      repository: UpdateRepository[F],
      service: UpdateService[F],
      processor: TaskProcessor[F]
  ): core.domain.update.UpdateGateway[F] = new:
    import Logger.*

    def update(dependencies: List[UpdateDependency]): F[Unit] =
      dependencies
        .traverse: dependency =>
          service
            .update(dependency)
            .flatMap:
              case Left(reason) => Logger[F].info(reason)
              case Right(_)     => Monad[F].unit
            .withLogContext("project", dependency.projectName)
            .withLogContext("dependency", dependency.dependencyName)
            .withLogContext("fromVersion", dependency.fromVersion)
            .withLogContext("toVersion", dependency.toVersion)
        .void

    def canUpdate(
        dependencies: List[DependencyToUpdate],
        projectId: UUID,
        sourceFile: String
    ): F[List[(DependencyToUpdate, Boolean)]] =
      val requests = dependencies.map: dependency =>
        UpdateRequest(projectId, dependency.name, dependency.latestVersion)
      repository.exist(requests).map: existingRequests =>
        dependencies.map: dependency =>
          val requestExists = existingRequests
            .find(_.dependencyName == dependency.name)
            .isDefined
          val canBeUpdated = dependency
            .currentVersion
            .map: currentVersion =>
              currentVersion != dependency.latestVersion
                && FileType.fromPath(sourceFile).isRight
            .getOrElse(false)
          (dependency, !requestExists && canBeUpdated)
