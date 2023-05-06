package application.services

import org.legogroup.woof.{ *, given }
import domain.Exporter
import domain.project.{ Project, ScanReport, ScanResultRepository }
import cats.*
import cats.implicits.*

trait ExportingService[F[_]]:
  def exportScanResults(projects: List[Project]): F[Unit]

object ExportingService:
  def make[F[_]: Monad: Logger](
      exporter: Exporter[F, ScanReport],
      repository: ScanResultRepository[F]
  ): ExportingService[F] = new ExportingService[F]:

    def exportScanResults(projects: List[Project]): F[Unit] =
      for
        _ <- Logger[F].info(
          s"Searching for stored scans of ${projects.length} projects"
        )
        scanReports <- repository.getLatestScanReports(projects.map(_.name))
        _           <- Logger[F].info(s"Found ${scanReports.length} scan reports")
        _           <- exporter.exportData(orderReports(projects, scanReports))
        _           <- Logger[F].info("Successfully exported scan results")
      yield ()

  private def orderReports(
      intendedOrder: List[Project],
      reports: List[ScanReport]
  ): List[ScanReport] =
    intendedOrder.map(project =>
      reports.find(_.projectName == project.name)
    ).collect {
      case Some(report) => report
    }
