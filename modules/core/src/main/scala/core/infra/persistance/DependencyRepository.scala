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
        existingDependencies <-
          NonEmptyList
            .fromList(dependencies.map(_.name))
            .map: names =>
              findDependencies(names).to[List].transact(xa)
            .getOrElse(List.empty.pure)
        existingDeps <- dependencies
          .map: dependency =>
            existingDependencies
              .find: (id, name) =>
                name == dependency.name
              .map: (id, name) =>
                id -> dependency
          .collect:
            case Some(id, dependency) => id -> dependency
          .traverse: (id, dependency) =>
            ResultToSave(id, dependency, timestamp)
        newDeps <- dependencies
          .map: dependency =>
            if existingDependencies
              .find: (id, name) =>
                name == dependency.name
              .isDefined
            then None
            else dependency.some
          .collect:
            case Some(dependency) => dependency
          .traverse: dependency =>
            ResultToSave(dependency, timestamp)
        allDeps = existingDeps ++ newDeps
        _ <-
          val inserts =
            for
              _ <- insertManyDependencies(newDeps.map(_.dependency))
              _ <- insertManyDependencyScans(allDeps.map(_.scan))
              _ <-
                insertManyVulnerabilities(allDeps.flatMap(_.vulnerabilities))
            yield ()
          inserts.transact(xa)
      yield allDeps.map(_.dependency)

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
        dependencyId: UUID,
        report: DependencyReport,
        timestamp: DateTime
    ): F[ResultToSave] =
      randomUUID.flatMap: scanId =>
        val dependency =
          ExistingDependency(dependencyId, report.name)
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
          .traverse: vulnerability =>
            randomUUID.map: id =>
              ExistingVulnerability(id, scanId, vulnerability)
          .map: vulnerabilities =>
            ResultToSave(dependency, scan, vulnerabilities)

    def apply[F[_]: UUIDGen: Monad](
        report: DependencyReport,
        timestamp: DateTime
    ): F[ResultToSave] =
      randomUUID.flatMap: dependencyId =>
        ResultToSave(dependencyId, report, timestamp)

  private object DependencyRepositorySQL:
    import sqlmappings.given

    case class RawDependency(
        id: UUID,
        name: String,
        currentVersion: Option[String],
        latestVersion: String,
        latestReleaseDate: Option[DateTime],
        notes: Option[String]
    )
    object RawDependency:
      def apply(
          id: UUID,
          dependency: DependencyReport
      ): RawDependency =
        RawDependency(
          id,
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
        INSERT INTO dependency (id, name)
        VALUES (?, ?)
      """
      Update[ExistingDependency](sql).updateMany(dependencies)

    def insertManyDependencyScans(
        scans: List[ExistingDependencyScan]
    ): ConnectionIO[Int] =
      val sql = """
        INSERT INTO dependency_scan (id, timestamp, dependency_id, current_version, latest_version, latest_release_date, notes)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      """
      Update[ExistingDependencyScan](sql).updateMany(scans)

    def insertManyVulnerabilities(
        vulnerabilities: List[ExistingVulnerability]
    ): ConnectionIO[Int] =
      val sql = """
          INSERT INTO vulnerability (id, dependency_scan_id, name)
          VALUES (?, ?, ?)
        """
      Update[ExistingVulnerability](sql).updateMany(vulnerabilities)

    def deleteByTimestamps(timestamps: NonEmptyList[DateTime]): Update0 =
      (sql"""
      DELETE 
      FROM dependency
      WHERE """ ++ Fragments.in(fr"timestamp", timestamps)).update

    def findDependencies(names: NonEmptyList[String]): Query0[(UUID, String)] =
      (sql"""
      SELECT id, name
      FROM dependency
      WHERE """ ++ Fragments.in(fr"name", names)).query[(UUID, String)]
