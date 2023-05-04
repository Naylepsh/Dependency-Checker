package infra.persistance

import domain.dependency.{ DependencyReport, DependencyRepository }
import java.util.UUID
import doobie.*
import doobie.implicits.*
import doobie.implicits.javatimedrivernative.*
import doobie.util.query.*
import org.joda.time.DateTime
import java.time.Instant
import cats.effect.MonadCancelThrow
import cats.effect.std.UUIDGen
import cats.effect.std.UUIDGen.randomUUID
import cats.*
import cats.implicits.*
import cats.effect.kernel.Sync

object DependencyRepository:
  def make[F[_]: MonadCancelThrow: UUIDGen: Sync](xa: Transactor[F])
      : DependencyRepository[F] = new DependencyRepository[F]:
    import DependencyRepositorySQL.*
    override def save(dependencies: List[DependencyReport]): F[Unit] =
      for
        now           <- Sync[F].delay(DateTime.now())
        crawlId       <- randomUUID
        dependencyIds <- dependencies.traverse(_ => randomUUID)
        _ <- dependencies.zip(dependencyIds).traverse {
          case (dependency, dependencyId) =>
            for
              vulnerabilityIds <-
                dependency.vulnerabilities.traverse(_ => randomUUID)
              _ <- insertDependency(
                dependencyId,
                now,
                dependency
              ).run.void.transact(xa)
              _ <- insertManyVulnerabilities(vulnerabilityIds.zip(
                dependency.vulnerabilities
              ).map(RawVulnerability(_, _, dependencyId))).transact(xa)
            yield ()
        }
      yield ()

  object DependencyRepositorySQL:
    given Get[UUID] = Get[String].map(UUID.fromString)
    given Put[UUID] = Put[String].contramap(_.toString)
    given Get[DateTime] = Get[Instant]
      .map(instant => DateTime(instant.toEpochMilli()))
    given Put[DateTime] = Put[Instant]
      .contramap(dt => Instant.ofEpochMilli(dt.getMillis()))

    def insertDependency(
        id: UUID,
        timestamp: DateTime,
        dependencyReport: DependencyReport
    ): Update0 =
      sql"""
        INSERT INTO dependency (id, timestamp, name, currentVersion, latestVersion, latestReleaseDate, notes)
        VALUES (
          ${id}, 
          ${timestamp}, 
          ${dependencyReport.name}, 
          ${dependencyReport.currentVersion}, 
          ${dependencyReport.latestVersion},
          ${dependencyReport.latestReleaseDate},
          ${dependencyReport.notes}
        )
      """.update

    case class RawVulnerability(id: UUID, name: String, dependencyId: UUID)

    def insertManyVulnerabilities(
        vulnerabilities: List[RawVulnerability]
    ) =
      val sql = """
          INSERT INTO vulnerability (id, name, dependencyId)
          VALUES (?, ?, ?)
        """
      Update[RawVulnerability](sql).updateMany(vulnerabilities)
