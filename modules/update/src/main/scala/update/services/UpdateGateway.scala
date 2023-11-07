package update.services

import cats.Applicative
import cats.syntax.all.*
import core.domain.update.DependencyToUpdate
import update.domain.FileType

object UpdateGateway:
  def make[F[_]: Applicative]: core.domain.update.UpdateGateway[F] = new:
    def canUpdate(
        dependency: DependencyToUpdate,
        sourceFile: String
    ): F[Boolean] =
      dependency
        .currentVersion
        .map: currentVersion =>
          currentVersion != dependency.latestVersion
            && FileType.fromPath(sourceFile).isRight
        .getOrElse(false)
        .pure
