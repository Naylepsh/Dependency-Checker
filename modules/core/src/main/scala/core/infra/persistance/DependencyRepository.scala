package core.infra.persistance

import java.time.Instant
import java.util.UUID

import cats.*
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.effect.kernel.Sync
import cats.effect.std.UUIDGen
import cats.effect.std.UUIDGen.randomUUID
import cats.implicits.*
import core.domain.dependency.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import org.joda.time.DateTime

object DependencyRepository:
  def make[F[_]: MonadCancelThrow: UUIDGen](xa: Transactor[F])
      : DependencyRepository[F] = new:
    import DependencyRepositorySQL.*

    def delete(timestamps: NonEmptyList[DateTime]): F[Unit] =
      deleteByTimestamps(timestamps).run.transact(xa).void

    def save(
        dependencies: List[DependencyReport],
        timestamp: DateTime
    ): F[List[ExistingDependency]] =
      for
        resultsToSave <-
          dependencies.traverse(dependency =>
            ResultToSave(dependency, timestamp)
          )
        _ <- save(resultsToSave).transact(xa)
      yield resultsToSave.map(_.dependency)

    private def save(resultsToSave: List[ResultToSave]) =
      for
        _ <- insertManyDependencies(resultsToSave.map(_.dependency))
        _ <- insertManyDependencyScans(resultsToSave.map(_.scan))
        _ <-
          insertManyVulnerabilities(resultsToSave.flatMap(_.vulnerabilities))
      yield ()

  private[DependencyRepository] case class ExistingDependencyScan(
      id: UUID,
      timestamp: DateTime,
      dependencyId: UUID,
      currentVersion: Option[String],
      latestVersion: String,
      latestReleaseDate: Option[DateTime],
      notes: Option[String] = None
  )

  private[DependencyRepository] case class ExistingVulnerability(
      id: UUID,
      dependencyScanId: UUID,
      name: String
  )

  private[DependencyRepository] case class ResultToSave(
      dependency: ExistingDependency,
      scan: ExistingDependencyScan,
      vulnerabilities: List[ExistingVulnerability]
  )
  private[DependencyRepository] object ResultToSave:
    def apply[F[_]: UUIDGen: Monad](
        report: DependencyReport,
        timestamp: DateTime
    ): F[ResultToSave] =
      (randomUUID, randomUUID).tupled.flatMap((dependencyId, scanId) =>
        val dependency =
          ExistingDependency(dependencyId, timestamp, report.name)
        val scan = ExistingDependencyScan(
          scanId,
          timestamp,
          dependencyId,
          report.currentVersion,
          report.latestVersion,
          report.latestReleaseDate,
          report.notes
        )
        report.vulnerabilities
          .traverse(vulnerability =>
            randomUUID.map(id =>
              ExistingVulnerability(id, scanId, vulnerability)
            )
          ).map(vulnerabilities =>
            ResultToSave(dependency, scan, vulnerabilities)
          )
      )

  private object DependencyRepositorySQL:
    import sqlmappings.given

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
        dependencies: List[ExistingDependency]
    ): ConnectionIO[Int] =
      val sql = """
        INSERT INTO dependency (id, timestamp, name)
        VALUES (?, ?, ?)
      """
      Update[ExistingDependency](sql).updateMany(dependencies)

    def insertManyDependencyScans(
        scans: List[ExistingDependencyScan]
    ): ConnectionIO[Int] =
      val sql = """
        INSERT INTO dependencyScan (id, timestamp, dependencyId, currentVersion, latestVersion, latestReleaseDate, notes)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      """
      Update[ExistingDependencyScan](sql).updateMany(scans)

    def insertManyVulnerabilities(
        vulnerabilities: List[ExistingVulnerability]
    ): ConnectionIO[Int] =
      val sql = """
          INSERT INTO vulnerability (id, dependencyScanId, name)
          VALUES (?, ?, ?)
        """
      Update[ExistingVulnerability](sql).updateMany(vulnerabilities)

    def deleteByTimestamps(timestamps: NonEmptyList[DateTime]): Update0 =
      (sql"""
      DELETE 
      FROM dependency
      WHERE """ ++ Fragments.in(fr"timestamp", timestamps)).update
