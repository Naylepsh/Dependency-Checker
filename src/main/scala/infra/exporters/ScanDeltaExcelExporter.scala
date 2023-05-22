package infra.exporters

import cats.effect.kernel.Sync
import cats.implicits.*
import domain.delta.{ DependencyDelta, ScanDelta }
import domain.{ Exporter, Grouped }
import spoiwo.model.*
import spoiwo.model.enums.CellFill
import spoiwo.natures.xlsx.Model2XlsxConversions.*
import domain.delta.PropertyDelta

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
        ) :: tableDescription :: groupRows

    private def makeDeltaRow(delta: DependencyDelta): Row =
      val (currentVersionLeft, currentVersionRight) =
        delta.delta.currentVersion
          .map(d =>
            val style = matchCurrentVersionDeltaToStyle(d)
            (
              Cell(d.left.getOrElse(notApplicable), style = style),
              Cell(d.right.getOrElse(notApplicable), style = style)
            )
          )
          .getOrElse(notApplicableCellPair)

      val (latestVersionLeft, latestVersionRight) =
        delta.delta.latestVersion
          .map(d =>
            val style = matchLatestVersionDeltaToStyle(d)
            (Cell(d.left, style = style), Cell(d.right, style = style))
          )
          .getOrElse(notApplicableCellPair)

      val (vulnerabilityCountLeft, vulnerabilityCountRight) =
        delta.delta.vulnerabilityCount
          .map(d =>
            val style = matchVulnerabilityCountDeltaToStyle(d)
            (
              Cell(d.left.toString, style = style),
              Cell(d.right.toString, style = style)
            )
          )
          .getOrElse(notApplicableCellPair)

      Row(
        Cell(delta.name),
        currentVersionLeft,
        currentVersionRight,
        latestVersionLeft,
        latestVersionRight,
        vulnerabilityCountLeft,
        vulnerabilityCountRight
      )

  private def matchCurrentVersionDeltaToStyle(
      delta: PropertyDelta.CurrentVersion
  ): CellStyle =
    (delta.left, delta.right) match
      case (None, None)    => Styles.noChange
      case (None, Some(_)) => Styles.upwardChange
      case (Some(_), None) => Styles.downwardChange
      case (Some(left), Some(right)) =>
        if left < right then Styles.upwardChange
        else if left == right then Styles.noChange
        else Styles.downwardChange

  private def matchLatestVersionDeltaToStyle(delta: PropertyDelta.LatestVersion)
      : CellStyle =
    if delta.left < delta.right then Styles.upwardChange
    else if delta.left == delta.right then Styles.noChange
    else Styles.downwardChange

  private def matchVulnerabilityCountDeltaToStyle(
      delta: PropertyDelta.VulnerabilityCount
  ): CellStyle =
    if delta.left < delta.right then Styles.upwardChange
    else if delta.left == delta.right then Styles.noChange
    else Styles.downwardChange

  private object Styles:
    val noChange = CellStyle(
      fillForegroundColor = Color(152, 160, 161),
      fillPattern = CellFill.Solid
    )
    val upwardChange = CellStyle(
      fillForegroundColor = Color(47, 158, 156),
      fillPattern = CellFill.Solid
    )
    val downwardChange = CellStyle(
      fillForegroundColor = Color(255, 99, 71),
      fillPattern = CellFill.Solid
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
  private val tableDescription =
    Row(style = headerStyle).withCellValues(columns)
  private def headerStyle           = CellStyle(font = Font(bold = true))
  private val notApplicable         = "-"
  private val notApplicablePair     = (notApplicable, notApplicable)
  private val notApplicableCell     = Cell("-", style = Styles.noChange)
  private val notApplicableCellPair = (notApplicableCell, notApplicableCell)
