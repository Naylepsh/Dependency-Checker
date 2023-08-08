package scanning.application

import cats.syntax.all.*
import java.util.UUID
import core.domain.project.ScanReport
import core.domain.project.ScanResultRepository
import cats.Functor

trait ScanReportService[F[_]]:
  def getLatestScan(projectName: String): F[Option[ScanReport]]

object ScanReportService:
  def make[F[_]: Functor](
      repository: ScanResultRepository[F]
  ): ScanReportService[F] = new:
    def getLatestScan(projectName: String): F[Option[ScanReport]] =
      repository.getLatestScanReports(List(projectName)).map:
        case report :: Nil => Some(report)
        case _             => None
