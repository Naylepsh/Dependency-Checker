package infra.exporters

import cats.effect.kernel.Sync
import cats.implicits.*
import domain.delta.{DependencyDelta, ScanDelta}
import domain.{Exporter, Grouped}
import spoiwo.model.*
import spoiwo.model.enums.CellFill
import spoiwo.natures.xlsx.Model2XlsxConversions.*

object ScanDeltaExcelExporter:
  def make[F[_]: Sync](path: String): Exporter[F, ScanDelta] = new:
    def exportData(scans: List[ScanDelta]): F[Unit] =
      val sheets = scans.foldRight(List.empty[Sheet])((scan, sheets) =>
        makeScanSheet(scan).fold(sheets)(_ :: sheets)
      )

      Workbook().withSheets(sheets).saveAsXlsx(path).pure

    private def makeScanSheet(scan: ScanDelta): Option[Sheet] =
      val rows = scan.dependenciesDeltas.flatMap(makeGroupRows)
      if rows.isEmpty then None
      else
        Sheet(name = scan.projectName)
          .withRows(rows)
          .withColumns(columns.map(_ => Column(autoSized = true)))
          .some

    private def makeGroupRows(group: Grouped[DependencyDelta]): List[Row] =
      val groupRows = group.items.collect {
        case delta if !delta.delta.isEmpty => makeDeltaRow(delta)
      }
      if groupRows.isEmpty then List.empty
      else
        Row(style = headerStyle).withCellValues(
          "Source:",
          group.groupName
        ) :: groupRows

    private def makeDeltaRow(delta: DependencyDelta): Row =
      val (currentVersionLeft, currentVersionRight) =
        delta.delta.currentVersion
          .map(d =>
            (d.left.getOrElse(notApplicable), d.right.getOrElse(notApplicable))
          )
          .getOrElse(notApplicablePair)

      val (latestVersionLeft, latestVersionRight) =
        delta.delta.latestVersion
          .map(d => (d.left, d.right))
          .getOrElse(notApplicablePair)

      val (vulnerabilityCountLeft, vulnerabilityCountRight) =
        delta.delta.vulnerabilityCount
          .map(d => (d.left.toString, d.right.toString))
          .getOrElse(notApplicablePair)

      Row(
        Cell(delta.name),
        Cell(currentVersionLeft),
        Cell(currentVersionRight),
        Cell(latestVersionLeft),
        Cell(latestVersionRight),
        Cell(vulnerabilityCountLeft),
        Cell(vulnerabilityCountRight)
      )

  private val columns = List(
    "Name",
    "Current Version (left)",
    "Current Version (right)",
    "Latest Version (left)",
    "Latest Version (right)",
    "Vulnerability Count (left)",
    "Vulnerability Count (right)"
  )
  private val headerStyle       = CellStyle(font = Font(bold = true))
  private val notApplicable     = "-"
  private val notApplicablePair = (notApplicable, notApplicable)
