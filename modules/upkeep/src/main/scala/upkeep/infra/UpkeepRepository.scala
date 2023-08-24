package upkeep.infra

import java.util.UUID

import cats.effect.MonadCancelThrow
import cats.effect.std.UUIDGen
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import upkeep.domain.*

object UpkeepRepository:
  /**
   * This package (and entire module tbh) has a case of bad naming.
   * Schemas changed and now projectId in this domain corresponds to gitlabId,
   * but this hasn't been propagated to the entire module.
   * TODO: Fix the naming
   */
  def make[F[_]: MonadCancelThrow: UUIDGen](
      xa: Transactor[F]
  ): UpkeepRepository[F, String] = new:
    override def save(request: UpkeepRequest[String]): F[Unit] =
      for
        id <- UUIDGen.randomUUID
        maybeProjectId <- UpkeepRepositorySQL
          .getProjectId(request.projectId)
          .option
          .transact(xa)
        _ <- maybeProjectId match
          case None => MonadCancelThrow[F].unit
          case Some(projectId) =>
            UpkeepRepositorySQL.insert(id, request, projectId)
              .run
              .transact(xa)
              .void
      yield ()

    override def isPending(
        projectId: String,
        dependencyName: String,
        updateToVersion: String
    ): F[Boolean] =
      UpkeepRepositorySQL.count(
        projectId,
        dependencyName,
        updateToVersion
      ).unique.map(_ > 0).transact(xa)

object UpkeepRepositorySQL:
  import persistance.sqlmappings.given

  def getProjectId(gitlabId: String) =
    sql"""
    SELECT project.id
    FROM project
    JOIN project_scan_config ON project_scan_config.project_id = project.id
    WHERE project_scan_config.gitlab_id = $gitlabId
    """.query[UUID]

  def insert(id: UUID, request: UpkeepRequest[?], projectId: UUID): Update0 =
    sql"""
    INSERT INTO upkeep_request (id, project_id, dependency_name, update_to_version, url)
    VALUES (
      $id, 
      ${projectId}, 
      ${request.dependencyName}, 
      ${request.updateToVersion}, 
      ${request.url})
    """.update

  def count(
      gitlabId: String,
      dependencyName: String,
      updateToVersion: String
  ): Query0[Int] =
    sql"""
    SELECT COUNT(*)
    FROM project
    JOIN project_scan_config ON project_scan_config.project_id = project.id
    JOIN upkeep_request ON upkeep_request.project_id = project.id
    WHERE project_scan_config.gitlab_id = $gitlabId
      AND dependency_name = $dependencyName
      AND update_to_version = $updateToVersion
    """.query[Int]
