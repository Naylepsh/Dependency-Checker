package update

import update.domain.UpdateRepository
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import java.util.UUID
import cats.effect.MonadCancelThrow
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import update.domain.UpdateAttempt

object repositories:
  object UpdateRepository:
    def make[F[_]: MonadCancelThrow: UUIDGen](xa: Transactor[F])
        : UpdateRepository[F] =
      new:
        def exists(
            projectId: UUID,
            dependencyName: String,
            toVersion: String
        ): F[Boolean] =
          UpdateRepositorySQL
            .exists(projectId, dependencyName, toVersion)
            .unique
            .transact(xa)

        def save(attempt: UpdateAttempt): F[UUID] =
          UUIDGen[F].randomUUID.flatMap: id =>
            UpdateRepositorySQL.save(id, attempt).run.transact(xa).as(id)

  private object UpdateRepositorySQL:
    import persistence.sqlmappings.given

    def exists(
        projectId: UUID,
        dependencyName: String,
        toVersion: String
    ): Query0[Boolean] =
      sql"""
      SELECT COUNT(*)
      FROM upkeep_request
      WHERE project_id = $projectId
      AND dependency_name = $dependencyName
      AND update_to_version = $toVersion
      """.query[Int].map(_ > 0)

    def save(
        id: UUID,
        attempt: UpdateAttempt
    ): Update0 =
      sql"""
      INSERT INTO upkeep_request (id, project_id, dependency_name, update_to_version, url)
      VALUES ($id, ${attempt.projectId}, ${attempt.dependencyName}, ${attempt.toVersion}, ${attempt.mergeRequestUrl})
      """.update
