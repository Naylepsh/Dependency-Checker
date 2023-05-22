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

    def save(
        results: List[ScanResult],
        timestamp: DateTime
    ): F[Unit] =
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
            records = dependencies.map(dependency =>
              ProjectDependency(
                timestamp,
                result.project.name,
                group.groupName,
                dependency.id
              )
            )
            _ <- insertMany(records).transact(xa)
          yield ()
        )
      ).void

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
      projectName: String,
      groupName: String,
      dependencyId: UUID
  )

  private[persistance] object ScanResultRepositorySQL:
    import sqlmappings.given

    def insertMany(projectDependencies: List[ProjectDependency])
        : ConnectionIO[Int] =
      val sql = """
      INSERT INTO projectDependency (
        timestamp,
        projectName,
        groupName,
        dependencyId)
      VALUES (?, ?, ?, ?)
      """
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
          projectDependency.projectName,
          projectDependency.groupName,
          dependency.id,
          dependency.name,
          dependencyScan.currentVersion,
          dependencyScan.latestVersion,
          dependencyScan.latestReleaseDate,
          dependencyScan.notes,
          vulnerability.name
        FROM projectDependency
        JOIN dependency ON dependency.id = projectDependency.dependencyId 
        JOIN dependencyScan ON dependencyScan.dependencyId = dependency.id
            AND dependencyScan.timestamp = $timestamp
        LEFT JOIN vulnerability ON vulnerability.dependencyScanId = dependencyScan.id
        WHERE """ ++ Fragments.in(
        fr"projectDependency.projectName",
        projectNames
      )).query[GetAllResult]

    def getLatestScansTimestamps(limit: Int): Query0[DateTime] =
      sql"""
      SELECT DISTINCT timestamp
      FROM dependencyScan
      ORDER BY timestamp DESC
      LIMIT $limit
      """.query[DateTime]
