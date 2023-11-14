package update.services

import java.util.UUID

import cats.Applicative
import cats.syntax.all.*
import core.domain.update.DependencyToUpdate
import update.domain.{ FileType, UpdateRepository }
import update.domain.UpdateRequest

object UpdateGateway:
  def make[F[_]: Applicative](repository: UpdateRepository[F])
      : core.domain.update.UpdateGateway[F] = new:
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
