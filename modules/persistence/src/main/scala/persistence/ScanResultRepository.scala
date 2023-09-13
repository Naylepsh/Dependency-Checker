package persistence

import java.util.UUID

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.implicits.*
import core.domain.Grouped
import core.domain.dependency.{
  DependencyLatestRelease,
  DependencyReport,
  DependencyRepository
}
import core.domain.project.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import org.joda.time.DateTime
import org.legogroup.woof.{ *, given }
import doobie.util.update.Update0

object ScanResultRepository:
  def make[F[_]: MonadCancelThrow: Logger](
      xa: Transactor[F],
      dependencyRepository: DependencyRepository[F]
  ): ScanResultRepository[F] = new:
    import ScanResultRepositorySQL.*

    def save(results: List[ScanResult], timestamp: DateTime): F[Unit] =
      results.traverse(result =>
        result.dependenciesReports.traverse(group =>
          for
            _ <- Logger[F].debug(
              s"Saving ${group.items.length} dependencies & vulnerabilities"
            )
            dependencies <- dependencyRepository.save(group.items, timestamp)
            _ <- Logger[F].debug(
              s"Saving ${dependencies.length} projectDependencies"
            )
            maybeProjectId <- getProjectId(result.project.name)
            _ <- maybeProjectId match
              case None => MonadCancelThrow[F].unit
              case Some(projectId) =>
                val records = dependencies.map: dependency =>
                  ProjectDependency(
                    timestamp,
                    projectId,
                    group.groupName,
                    dependency.id
                  )
                insertMany(records).transact(xa).void
          yield ()
        )
      ).void

    private def getProjectId(projectName: String): F[Option[UUID]] =
      ScanResultRepositorySQL.getProjectId(projectName).option.transact(xa)

    def getScanReports(
        projectNames: List[String],
        timestamp: DateTime
    ): F[List[ScanReport]] =
      // TODO: Fix it I guess? Although it's only being used by delta service which isn't really hooked up to anything right now
      List.empty.pure
      // NonEmptyList.fromList(projectNames).fold(List.empty.pure): names =>
      //   dependencyRepository.findLatestReleases(projectNames).flatMap:
      //     releases =>
      //       getAll(names, timestamp)
      //         .to[List]
      //         .map: results =>
      //           GetAllResult.toDomain(results, releases)
      //         .transact(xa)

    def getLatestScansTimestamps(limit: Int): F[List[DateTime]] =
      if limit > 0 then
        ScanResultRepositorySQL.getLatestScansTimestamps(limit)
          .to[List]
          .transact(xa)
      else List.empty.pure

    def getLatestScanReport(projectName: String): F[Option[ScanReport]] =
      ScanResultRepositorySQL
        .getScanTimestamps(projectName, limit = 1)
        .option
        .transact(xa)
        .flatMap:
          case None => None.pure
          case Some(timestamp) =>
            for
              dependencyIds <- ScanResultRepositorySQL
                .getDependenciesOfProject(projectName, timestamp)
                .to[List]
                .transact(xa)
              releases <- dependencyRepository.findLatestReleases(dependencyIds)
              report <- getAll(NonEmptyList.of(projectName), timestamp)
                .to[List]
                .map: results =>
                  GetAllResult.toDomain(results, releases)
                .map(_.headOption)
                .transact(xa)
            yield report

    def getLatestScanReports(projectNames: List[String]): F[List[ScanReport]] =
      for
        _         <- Logger[F].info("Starting...")
        timestamp <- getLatestScanTimestamp()
        _         <- Logger[F].info(s"Last scan happened at $timestamp")
        reports <-
          timestamp.fold(Applicative[F].pure(List.empty))(timestamp =>
            getScanReports(projectNames, timestamp)
          )
        _ <- Logger[F].info(s"Reports count: ${reports.length}")
      yield reports

    def getVulnerabilitySummary(projectNames: NonEmptyList[String])
        : F[List[VulnerabilitySummary]] =
      ScanResultRepositorySQL
        .getVulnerabilitySummary(projectNames)
        .to[List]
        .transact(xa)

    def getVulnerabilitiesSince(time: DateTime): F[List[ProjectVulnerability]] =
      ScanResultRepositorySQL
        .getVulnerabilitiesSince(time)
        .to[List]
        .transact(xa)

    def delete(timestamps: NonEmptyList[DateTime]): F[Unit] =
      dependencyRepository.delete(timestamps)

    def deleteOld(projectName: String): F[Unit] =
      val getLatestTimestamp =
        ScanResultRepositorySQL
          .getScanTimestamps(projectName, limit = 1)
          .option
          .transact(xa)
      (getLatestTimestamp, getProjectId(projectName)).tupled.flatMap:
        case (Some(timestamp), Some(id)) =>
          /**
           * TODO: Delete dependencies that are no longer attached any project?
           */
          ScanResultRepositorySQL
            .deleteOldDependencies(id, timestamp)
            .run
            .transact(xa)
            .void
        case _ => Applicative[F].unit

  case class ProjectDependency(
      timestamp: DateTime,
      projectId: UUID,
      groupName: String,
      dependencyId: UUID
  )

  object ScanResultRepositorySQL:
    import persistence.sqlmappings.given

    given Read[VulnerabilitySummary] =
      Read[(String, Int)].map: (projectName, vulnerabilityCount) =>
        VulnerabilitySummary(projectName, vulnerabilityCount)

    def getProjectId(projectName: String) =
      sql"""
      SELECT id
      FROM project
      WHERE project.name = $projectName
      """.query[UUID]

    def insertMany(projectDependencies: List[ProjectDependency])
        : ConnectionIO[Int] =
      val sql = s"""
      INSERT INTO project_dependency (
        timestamp,
        project_id,
        group_name,
        dependency_id
      )
      VALUES (?, ?, ?, ?)"""
      Update[ProjectDependency](sql).updateMany(projectDependencies)

    case class GetAllResult(
        projectName: String,
        groupName: String,
        dependencyId: String,
        dependencyName: String,
        dependencyVersion: Option[String],
        dependencyReleaseDate: Option[DateTime],
        dependencyNotes: Option[String],
        dependencyVulnerability: Option[String]
    )
    object GetAllResult:
      def toDomain(
          results: List[GetAllResult],
          latestReleases: List[DependencyLatestRelease]
      ): List[ScanReport] =
        results.groupBy(_.projectName).map((projectName, projectResults) =>
          val reports = projectResults
            .groupBy(_.groupName)
            .map((groupName, groupResults) =>
              // TODO: Instead of mapping and collecting, just foldRight
              val dependencies = groupResults
                .groupBy(_.dependencyId)
                .map: (dependencyId, results) =>
                  val vulnerabilities = results
                    .filter(_.dependencyVulnerability.isDefined)
                    .map(_.dependencyVulnerability.get)
                  // Safe, because groupBy guaranteed results to be non-empty
                  val result = results.head
                  val found = latestReleases
                    .find: release =>
                      release.name == result.dependencyName
                  latestReleases
                    .find: release =>
                      release.name == result.dependencyName
                    .map: release =>
                      DependencyReport(
                        result.dependencyName,
                        result.dependencyVersion,
                        release.version,
                        result.dependencyReleaseDate,
                        release.releaseDate.some,
                        vulnerabilities,
                        result.dependencyNotes
                      )
                .toList
                .collect:
                  case Some(result) => result
              Grouped(groupName, dependencies)
            )
          ScanReport(projectName, reports.toList)
        ).toList

    def getDependenciesOfProject(
        projectName: String,
        timestamp: DateTime
    ): Query0[UUID] =
      sql"""
      SELECT dependency_id
      FROM project
      JOIN project_dependency ON project_dependency.project_id = project.id
      WHERE project.name = $projectName
      AND timestamp = $timestamp
      """.query[UUID]

    def getAll(
        projectNames: NonEmptyList[String],
        timestamp: DateTime
    ): Query0[GetAllResult] =
      (sql"""
        SELECT
          project.name,
          project_dependency.group_name,
          dependency.id,
          dependency.name,
          dependency.version,
          dependency.release_date,
          dependency.notes,
          vulnerability.name
        FROM project_dependency
        JOIN project ON project.id = project_dependency.project_id
        JOIN dependency ON dependency.id = project_dependency.dependency_id 
            AND project_dependency.timestamp = $timestamp
        LEFT JOIN vulnerability ON vulnerability.dependency_id = dependency.id
        WHERE """ ++ Fragments.in(
        fr"project.name",
        projectNames
      )).query[GetAllResult]

    def getLatestScansTimestamps(limit: Int): Query0[DateTime] =
      sql"""
      SELECT DISTINCT timestamp
      FROM dependency_scan
      ORDER BY timestamp DESC
      LIMIT $limit
      """.query[DateTime]

    def getScanTimestamps(projectName: String, limit: Int): Query0[DateTime] =
      sql"""
      SELECT DISTINCT timestamp
      FROM project
      JOIN project_dependency ON project_dependency.project_id = project.id
      WHERE project.name = $projectName
      ORDER BY timestamp DESC
      LIMIT $limit
      """.query[DateTime]

    def deleteOldDependencies(
        projectId: UUID,
        maxTimestamp: DateTime
    ): Update0 =
      // delete everything but the latest dependencies
      sql"""
      DELETE
      FROM project_dependency
      WHERE project_id = $projectId
      AND project_dependency.timestamp < $maxTimestamp
      """.update

    def getVulnerabilitySummary(projectNames: NonEmptyList[String]) =
      sql"""
      SELECT project.name, COUNT(vulnerability.name)
      FROM project
      LEFT JOIN project_dependency ON project_dependency.project_id = project.id
      LEFT JOIN vulnerability ON vulnerability.dependency_id = project_dependency.dependency_id
      GROUP BY project.name
      """.query[VulnerabilitySummary]

    given Read[ProjectVulnerability] =
      Read[(String, String, Option[String], String)].map:
        (vulnName, depName, depVersion, projectName) =>
          ProjectVulnerability(vulnName, depName, depVersion, projectName)

    def getVulnerabilitiesSince(time: DateTime) =
      sql"""
      SELECT vulnerability.name, dependency.name, dependency.version, project.name
      FROM vulnerability
      JOIN dependency ON dependency.id = vulnerability.dependency_id
      JOIN project_dependency ON project_dependency.dependency_id = dependency.id
      JOIN project ON project.id = project_dependency.project_id
      WHERE vulnerability.date_created > $time
      """.query[ProjectVulnerability]
