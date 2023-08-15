package core.domain

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

  case class ScanResult(
      project: Project,
      dependenciesReports: List[Grouped[DependencyReport]]
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
    def delete(timestamps: NonEmptyList[DateTime]): F[Unit]

  case class ScanReport(
      projectName: String,
      dependenciesReports: List[Grouped[DependencyReport]]
  )
