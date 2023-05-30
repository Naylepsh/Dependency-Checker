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

    override def isPending(request: UpkeepRequest[String]): F[Boolean] =
      UpkeepRepositorySQL.count(request).unique.map(_ > 0).transact(xa)

object UpkeepRepositorySQL:
  import core.infra.persistance.sqlmappings.given

  def insert(id: UUID, request: UpkeepRequest[String]): Update0 =
    sql"""
    INSERT INTO upkeepRequest (id, projectId, dependencyName, updateToVersion)
    VALUES ($id, ${request.projectId}, ${request.dependencyName}, ${request.updateToVersion})
    """.update

  def count(request: UpkeepRequest[String]): Query0[Int] =
    sql"""
    SELECT COUNT(*)
    FROM upkeepRequest
    WHERE projectId = ${request.projectId}
      AND dependencyName = ${request.dependencyName}
      AND updateToVersion = ${request.updateToVersion}
    """.query[Int]
