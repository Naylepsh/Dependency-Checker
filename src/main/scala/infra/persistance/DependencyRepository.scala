package infra.persistance

import java.time.Instant
import java.util.UUID

import cats.*
import cats.effect.MonadCancelThrow
import cats.effect.kernel.Sync
import cats.effect.std.UUIDGen
import cats.effect.std.UUIDGen.randomUUID
import cats.implicits.*
import domain.dependency.{ DependencyReport, DependencyRepository }
import doobie.*
import doobie.implicits.*
import doobie.implicits.javatimedrivernative.*
import doobie.util.query.*
import org.joda.time.DateTime

object DependencyRepository:
  def make[F[_]: MonadCancelThrow: UUIDGen: Sync](xa: Transactor[F])
      : DependencyRepository[F] = new DependencyRepository[F]:
    import DependencyRepositorySQL.*
    override def save(dependencies: List[DependencyReport]): F[Unit] =
      for
        now           <- Sync[F].delay(DateTime.now())
        crawlId       <- randomUUID
        dependencyIds <- dependencies.traverse(_ => randomUUID)
        _             <- saveDependencies(dependencies, dependencyIds, now)
        _             <- saveVulnerabilities(dependencies, dependencyIds)
      yield ()

    private def saveDependencies(
        dependencies: List[DependencyReport],
        dependencyIds: List[UUID],
        now: DateTime
    ): F[Unit] =
      val records = dependencies.zip(dependencyIds).map {
        case (dependency, id) => RawDependency(id, now, dependency)
      }
      insertManyDependencies(records).transact(xa).void

    private def saveVulnerabilities(
        dependencies: List[DependencyReport],
        dependencyIds: List[UUID]
    ): F[Unit] =
      for
        records <- dependencies.zip(dependencyIds).traverse {
          case (dependency, dependencyId) =>
            dependency.vulnerabilities.traverse(vuln =>
              randomUUID.map(id => (id, vuln, dependencyId))
            )
        }.map(_.flatten.map(RawVulnerability.apply))
        _ <- insertManyVulnerabilities(records).transact(xa)
      yield ()

  object DependencyRepositorySQL:
    given Get[UUID] = Get[String].map(UUID.fromString)
    given Put[UUID] = Put[String].contramap(_.toString)
    given Get[DateTime] = Get[Instant]
      .map(instant => DateTime(instant.toEpochMilli()))
    given Put[DateTime] = Put[Instant]
      .contramap(dt => Instant.ofEpochMilli(dt.getMillis()))

    case class RawDependency(
        id: UUID,
        timestamp: DateTime,
        name: String,
        currentVersion: Option[String],
        latestVersion: String,
        latestReleaseDate: Option[DateTime],
        notes: Option[String]
    )
    object RawDependency:
      def apply(
          id: UUID,
          timestamp: DateTime,
          dependency: DependencyReport
      ): RawDependency =
        RawDependency(
          id,
          timestamp,
          dependency.name,
          dependency.currentVersion,
          dependency.latestVersion,
          dependency.latestReleaseDate,
          dependency.notes
        )

    def insertManyDependencies(
        dependencies: List[RawDependency]
    ): ConnectionIO[Int] =
      val sql = """
        INSERT INTO dependency (id, timestamp, name, currentVersion, latestVersion, latestReleaseDate, notes)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      """
      Update[RawDependency](sql).updateMany(dependencies)

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
    ): ConnectionIO[Int] =
      val sql = """
          INSERT INTO vulnerability (id, name, dependencyId)
          VALUES (?, ?, ?)
        """
      Update[RawVulnerability](sql).updateMany(vulnerabilities)
