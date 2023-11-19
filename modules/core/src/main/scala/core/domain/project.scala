package core.domain

import java.util.UUID

import cats.*
import cats.data.NonEmptyList
import cats.implicits.*
import com.github.nscala_time.time.Imports.*
import org.joda.time.{ DateTime, Days }

import dependency.*
import vulnerability.*

object project:
  case class Project(repositoryId: String, name: String)
  case class ExistingProject(id: UUID, repositoryId: String, name: String):
    def toProject: Project = Project(repositoryId, name)

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
  case class ExistingProjectScanConfig(
      id: UUID,
      project: ExistingProject,
      sources: List[DependencySource],
      enabled: Boolean,
      branch: String
  ):
    def toProjectScanConfig: ProjectScanConfig =
      ProjectScanConfig(project.toProject, sources, enabled, branch)

  trait ProjectScanConfigRepository[F[_]]:
    def all: F[List[ExistingProjectScanConfig]]
    def findByProjectName(projectName: String)
        : F[Option[ExistingProjectScanConfig]]
    def save(config: ProjectScanConfig): F[UUID]
    def setEnabled(projectName: String, enabled: Boolean): F[Unit]
    def delete(projectId: UUID): F[Unit]

  case class ScanResult(
      project: Project,
      dependenciesReports: List[Grouped[DependencyReport]]
  )

  case class VulnerabilitySummary(projectName: String, vulnerabilityCount: Int)

  case class ProjectVulnerability(
      vulnerabilityName: String,
      vulnerabilitySeverity: Option[VulnerabilitySeverity],
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
    def getVulnerabilitiesOfUnknownSeverity: F[List[String]]
    def setVulnerabilitySeverity(
        vulnerabilityName: String,
        severity: VulnerabilitySeverity
    ): F[Unit]
    def delete(timestamps: NonEmptyList[DateTime]): F[Unit]
    def deleteOld(projectName: String): F[Unit]

  case class ScanReport(
      projectId: UUID,
      projectName: String,
      dependenciesReports: List[Grouped[DependencyScanReport]]
  )
  object ScanReport:
    def sortGroups(
        compare: (DependencyScanReport, DependencyScanReport) => Int,
        report: ScanReport
    ): ScanReport =
      val lt: (DependencyScanReport, DependencyScanReport) => Boolean =
        (a, b) => compare(a, b) < 0
      report.copy(dependenciesReports =
        report
          .dependenciesReports
          .map: group =>
            group.copy(items = group.items.sortWith(lt))
      )
