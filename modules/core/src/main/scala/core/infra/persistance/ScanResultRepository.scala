package core.infra.persistance

import java.util.UUID

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.implicits.*
import core.domain.Grouped
import core.domain.dependency.{ DependencyReport, DependencyRepository }
import core.domain.project.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import org.joda.time.DateTime
import org.legogroup.woof.{ *, given }

object ScanResultRepository:
  def make[F[_]: MonadCancelThrow: Logger](
      xa: Transactor[F],
      dependencyRepository: DependencyRepository[F]
  ): ScanResultRepository[F] = new:
    import ScanResultRepositorySQL.*

    def delete(timestamps: NonEmptyList[DateTime]): F[Unit] =
      dependencyRepository.delete(timestamps)

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
      NonEmptyList.fromList(projectNames).fold(List.empty.pure)(names =>
        getAll(names, timestamp)
          .to[List]
          .map(GetAllResult.toDomain)
          .transact(xa)
      )

    def getLatestScansTimestamps(limit: Int): F[List[DateTime]] =
      if limit > 0 then
        ScanResultRepositorySQL.getLatestScansTimestamps(limit)
          .to[List]
          .transact(xa)
      else List.empty.pure

    def getLatestScanReport(projectName: String): F[Option[ScanReport]] =
      ScanResultRepositorySQL
        .getLatestScanTimestamp(projectName)
        .option
        .flatMap: timestamp =>
          timestamp.traverse: timestamp =>
            getAll(NonEmptyList.of(projectName), timestamp)
              .to[List]
              .map(GetAllResult.toDomain)
              .map(_.headOption)
        .map(_.flatten)
        .transact(xa)

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

  private[persistance] case class ProjectDependency(
      timestamp: DateTime,
      projectId: UUID,
      groupName: String,
      dependencyId: UUID
  )

  private[persistance] object ScanResultRepositorySQL:
    import sqlmappings.given

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
        dependency_id)
      VALUES (?, ?, ?, ?)"""
      Update[ProjectDependency](sql).updateMany(projectDependencies)

    case class GetAllResult(
        projectName: String,
        groupName: String,
        dependencyId: String,
        dependencyName: String,
        dependencyCurrentVersion: Option[String],
        dependencyLatestVersion: String,
        dependencyLatestReleaseDate: Option[DateTime],
        dependencyNotes: Option[String],
        dependencyVulnerability: Option[String]
    )
    object GetAllResult:
      def toDomain(results: List[GetAllResult]): List[ScanReport] =
        results.groupBy(_.projectName).map((projectName, projectResults) =>
          val reports = projectResults
            .groupBy(_.groupName)
            .map((groupName, groupResults) =>
              val dependencies = groupResults
                .groupBy(_.dependencyId)
                .map((dependencyId, results) =>
                  val vulnerabilities = results
                    .filter(_.dependencyVulnerability.isDefined)
                    .map(_.dependencyVulnerability.get)
                  // Safe, because groupBy guaranteed results to be non-empty
                  val result = results.head
                  DependencyReport(
                    result.dependencyName,
                    result.dependencyCurrentVersion,
                    result.dependencyLatestVersion,
                    result.dependencyLatestReleaseDate,
                    vulnerabilities,
                    result.dependencyNotes
                  )
                ).toList
              Grouped(groupName, dependencies)
            )
          ScanReport(projectName, reports.toList)
        ).toList

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
          dependency_scan.current_version,
          dependency_scan.latest_version,
          dependency_scan.latest_release_date,
          dependency_scan.notes,
          vulnerability.name
        FROM project_dependency
        JOIN project ON project.id = project_dependency.project_id
        JOIN dependency ON dependency.id = project_dependency.dependency_id 
        JOIN dependency_scan ON dependency_scan.dependency_id = dependency.id
            AND dependency_scan.timestamp = $timestamp
        LEFT JOIN vulnerability ON vulnerability.dependency_scan_id = dependency_scan.id
        WHERE """ ++ Fragments.in(
        fr"project_dependency.project_name",
        projectNames
      )).query[GetAllResult]

    def getLatestScansTimestamps(limit: Int): Query0[DateTime] =
      sql"""
      SELECT DISTINCT timestamp
      FROM dependency_scan
      ORDER BY timestamp DESC
      LIMIT $limit
      """.query[DateTime]

    def getLatestScanTimestamp(projectName: String): Query0[DateTime] =
      sql"""
      SELECT DISTINCT timestamp
      FROM project
      JOIN project_dependency ON project_dependency.project_id = project.id
      WHERE project.name = $projectName
      ORDER BY timestamp DESC
      LIMIT 1
      """.query[DateTime]
