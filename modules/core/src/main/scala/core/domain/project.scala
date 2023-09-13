package core.domain

import java.util.UUID

import cats.*
import cats.data.NonEmptyList
import cats.implicits.*
import org.joda.time.DateTime

import dependency.*

object project:
  case class Project(id: String, name: String)

  case class ProjectDependencies(
      project: Project,
      dependencies: List[Grouped[Dependency]]
  )

  case class ProjectScanConfig(
      project: Project,
      sources: List[DependencySource],
      enabled: Boolean,
      branch: String
  )

  trait ProjectScanConfigRepository[F[_]]:
    def all: F[List[ProjectScanConfig]]
    def save(config: ProjectScanConfig): F[UUID]
    def setEnabled(projectName: String, enabled: Boolean): F[Unit]

  case class ScanResult(
      project: Project,
      dependenciesReports: List[Grouped[DependencyReport]]
  )

  case class VulnerabilitySummary(projectName: String, vulnerabilityCount: Int)

  case class ProjectVulnerability(
      vulnerabilityName: String,
      dependencyName: String,
      dependencyVersion: Option[String],
      projectName: String
  )

  trait ScanResultRepository[F[_]: Functor]:
    def save(results: List[ScanResult], timestamp: DateTime): F[Unit]
    def getScanReports(
        projectNames: List[String],
        timestamp: DateTime
    ): F[List[ScanReport]]
    def getLatestScanReport(projectName: String): F[Option[ScanReport]]
    def getLatestScanReports(projectNames: List[String]): F[List[ScanReport]]
    def getLatestScanTimestamp(): F[Option[DateTime]] =
      getLatestScansTimestamps(1).map(_.headOption)
    def getLatestScansTimestamps(limit: Int): F[List[DateTime]]
    def getVulnerabilitySummary(projectNames: NonEmptyList[String])
        : F[List[VulnerabilitySummary]]
    def getVulnerabilitiesSince(time: DateTime): F[List[ProjectVulnerability]]
    def delete(timestamps: NonEmptyList[DateTime]): F[Unit]
    def deleteOld(projectName: String): F[Unit]

  case class ScanReport(
      projectName: String,
      dependenciesReports: List[Grouped[DependencyReport]]
  )
  object ScanReport:
    def sortGroups(
        compare: (DependencyReport, DependencyReport) => Int,
        report: ScanReport
    ): ScanReport =
      val lt: (DependencyReport, DependencyReport) => Boolean =
        (a, b) => compare(a, b) < 0
      report.copy(dependenciesReports =
        report
          .dependenciesReports
          .map: group =>
            group.copy(items = group.items.sortWith(lt))
      )
