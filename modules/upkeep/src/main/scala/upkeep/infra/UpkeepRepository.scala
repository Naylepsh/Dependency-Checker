package upkeep.infra

import cats.effect.MonadCancelThrow
import cats.implicits.*
import cats.effect.std.UUIDGen
import upkeep.domain.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import java.util.UUID

object UpkeepRepository:
  def make[F[_]: MonadCancelThrow: UUIDGen](
      xa: Transactor[F]
  ): UpkeepRepository[F, String] = new:
    override def save(request: UpkeepRequest[String]): F[Unit] =
      UUIDGen.randomUUID.flatMap(id =>
        UpkeepRepositorySQL.insert(id, request).run.transact(xa).void
      )

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
  import core.infra.persistance.sqlmappings.given

  def insert(id: UUID, request: UpkeepRequest[String]): Update0 =
    sql"""
    INSERT INTO upkeepRequest (id, projectId, dependencyName, updateToVersion, url)
    VALUES (
      $id, 
      ${request.projectId}, 
      ${request.dependencyName}, 
      ${request.updateToVersion}, 
      ${request.url})
    """.update

  def count(
      projectId: String,
      dependencyName: String,
      updateToVersion: String
  ): Query0[Int] =
    sql"""
    SELECT COUNT(*)
    FROM upkeepRequest
    WHERE projectId = $projectId
      AND dependencyName = $dependencyName
      AND updateToVersion = $updateToVersion
    """.query[Int]
