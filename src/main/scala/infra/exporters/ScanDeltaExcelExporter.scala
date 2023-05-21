package infra.exporters

import cats.implicits.*
import cats.effect.kernel.Sync
import domain.Exporter
import domain.delta.ScanDelta
import spoiwo.model.*
import spoiwo.model.enums.CellFill
import spoiwo.natures.xlsx.Model2XlsxConversions.*

object ScanDeltaExcelExporter:
  def make[F[_]: Sync](path: String): Exporter[F, ScanDelta] = new:
    def exportData(deltas: List[ScanDelta]): F[Unit] =
      val workbook = Workbook().withSheets(List.empty)
      workbook.saveAsXlsx(path).pure

    
