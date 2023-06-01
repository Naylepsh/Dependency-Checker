package core.application.services

import cats.*
import cats.implicits.*
import core.domain.Exporter
import core.domain.project.{ Project, ScanReport, ScanResultRepository }
import org.legogroup.woof.{ *, given }

trait ScanReportExportService[F[_]]:
  def exportScanResults(projects: List[Project]): F[Unit]

object ScanReportExportService:
  def make[F[_]: Monad: Logger](
      exporter: Exporter[F, ScanReport],
      repository: ScanResultRepository[F]
  ): ScanReportExportService[F] = new ScanReportExportService[F]:

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
    intendedOrder.flatMap(project =>
      reports.find(_.projectName == project.name)
    )
