package scanning.application.services

import scala.annotation.tailrec

import cats.*
import cats.data.NonEmptyList
import cats.effect.std.*
import cats.implicits.*
import core.domain.*
import core.domain.dependency.*
import core.domain.project.*
import core.domain.registry.ProjectScanConfig
import org.joda.time.DateTime
import org.legogroup.woof.{ *, given }
import scanning.domain.Source

trait ScanningService[F[_]]:
  def scan(project: ProjectScanConfig): F[Unit]
  def scan(projects: List[ProjectScanConfig]): F[Unit]
  def getLatestScansTimestamps(limit: Int): F[List[DateTime]]
  def getLatestScan(projectName: String): F[Option[ScanReport]] 
  def deleteScans(timestamps: NonEmptyList[DateTime]): F[Unit]

object ScanningService:
  def make[F[_]: Monad: Logger: Parallel: Time](
      source: Source[F, ProjectScanConfig],
      scanner: DependencyScanner[F],
      repository: ScanResultRepository[F],
  ): ScanningService[F] = new:
    def deleteScans(timestamps: NonEmptyList[DateTime]): F[Unit] =
      Logger[F].info(s"Deleting scans of ${timestamps.length} timestamps")
        >> repository.delete(timestamps)
        >> Logger[F].info("Successfully deleted the scan(s)")

    def scan(project: ProjectScanConfig): F[Unit] =
      for
        _ <- Logger[F].info(s"Scanning dependencies of ${project.name}")
        projectDependencies <- source
          .extract(project)
          .map: dependencies =>
            ProjectDependencies(Project(project.id, project.name), dependencies)
        allDependencies = projectDependencies
          .dependencies
          .flatMap(_.items)
          .distinct
        _ <- Logger[F].info(
          s"Checking the details of ${allDependencies.length} dependencies"
        )
        details <- scanner.getDetails(allDependencies)
        report = buildReport(buildDetailsMap(details))(projectDependencies)
        _   <- Logger[F].info("Saving the scan results...")
        now <- Time[F].currentDateTime
        _   <- repository.save(List(report), now)
        _   <- Logger[F].info("Done with the scan")
      yield ()

    def scan(projects: List[ProjectScanConfig]): F[Unit] =
      for
        _ <- Logger[F].info(
          s"Scanning dependencies of ${projects.length} projects..."
        )
        projectsDependencies <- projects
          .parTraverse: project =>
            source.extract(project)
              .map: dependencies =>
                ProjectDependencies(
                  Project(project.id, project.name),
                  dependencies
                )
        dependencies = projectsDependencies
          .flatMap(_.dependencies.flatMap(_.items))
          .distinct
        _ <- Logger[F].info(
          s"Checking the details of ${dependencies.length} dependencies..."
        )
        details <- scanner.getDetails(dependencies)
        _       <- Logger[F].info("Building the report...")
        reports =
          projectsDependencies.map(buildReport(buildDetailsMap(details)))
        _   <- Logger[F].info("Saving the scan results...")
        now <- Time[F].currentDateTime
        _   <- repository.save(reports, now)
        _   <- Logger[F].info("Done with the scan")
      yield ()

    def getLatestScansTimestamps(limit: Int): F[List[DateTime]] =
      repository.getLatestScansTimestamps(limit)

    def getLatestScan(projectName: String): F[Option[ScanReport]] =
      repository.getLatestScanReports(List(projectName)).map:
        case report :: Nil => Some(report)
        case _             => None

  private val latestKey = "LATEST"

  private type DetailsMap = Map[String, Map[String, DependencyDetails]]

  private def getDetails(
      details: DetailsMap
  )(name: String, version: String): Option[DependencyDetails] =
    details.get(name).flatMap(_.get(version))

  private def buildDetailsMap(
      dependenciesDetails: List[DependencyDetails]
  ): DetailsMap =
    dependenciesDetails
      .groupBy(_.name)
      .map {
        case (name, details) =>
          val orderedByVersion = details.sortWith(_.ofVersion > _.ofVersion)
          val latest           = orderedByVersion.head.copy(ofVersion = latestKey)

          name -> (latest :: orderedByVersion).map(d => d.ofVersion -> d).toMap
      }
      .toMap

  private def buildReport(details: DetailsMap)(
      projectDependencies: ProjectDependencies
  ): ScanResult =
    val detailsOf = getDetails(details)

    @tailrec
    def inner(
        dependencies: List[Dependency],
        report: List[DependencyReport]
    ): List[DependencyReport] =
      dependencies match
        case head :: next =>
          detailsOf(head.name, head.currentVersion.getOrElse(latestKey)) match
            case Some(detail) =>
              val notes = detail.minLanguageVersion.map(version =>
                s"min. language version: $version"
              )

              inner(next, DependencyReport(head, detail, notes) :: report)

            case None => inner(next, report)

        case Nil => report

    val report = projectDependencies.dependencies.map(dependencyGroup =>
      Grouped(
        dependencyGroup.groupName,
        inner(dependencyGroup.items, List.empty)
      )
    )
    ScanResult(projectDependencies.project, report)