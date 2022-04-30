package Dependencies

import spoiwo.model.{Row, Sheet, Font, Workbook, CellStyle}
import spoiwo.natures.xlsx.Model2XlsxConversions._
import org.apache.poi.ss.usermodel.Cell
import scala.util.Success
import scala.util.Failure
import spoiwo.model.Color
import spoiwo.model.enums.CellFill

trait SheetExporter[A] {
  def createSheet(source: A): Sheet
}

class RepositoryDependenciesSheetExporter
    extends SheetExporter[RepositoryDependencies] {
  private val headerStyle = CellStyle(font = Font(bold = true))

  private def chooseStyle(dependency: Dependency): CellStyle = {
    val versionDifference = for {
      current <- dependency.currentVersion
      latest <- dependency.latestVersion
    } yield calculateVersionDifference(current, latest)

    val optionalStyle = versionDifference.map(_ match {
      case Success(Some(diff)) => matchVersionDiffToStyle(diff)
      case Success(None) =>
        CellStyle(
          fillPattern = CellFill.Solid,
          fillForegroundColor = Color.Green
        )
      case Failure(errror) => CellStyle()
    })

    optionalStyle.getOrElse(CellStyle())
  }

  private def matchVersionDiffToStyle(
      versionDiff: VersionDifference
  ): CellStyle = CellStyle(
    fillForegroundColor = versionDiff match {
      case VersionDifference.Major => Color.Red
      case VersionDifference.Minor => Color.Yellow
      case VersionDifference.Patch => Color.LightGreen
    },
    fillPattern = CellFill.Solid
  )

  def createSheet(repoDependencies: RepositoryDependencies): Sheet = {
    val headerRow = Row(style = headerStyle)
      .withCellValues("Name", "Current Version", "Latest Version")
    val dataRows = repoDependencies.dependencies.map(d =>
      Row(style = chooseStyle(d)).withCellValues(
        d.name,
        d.currentVersion.getOrElse(""),
        d.latestVersion.getOrElse("")
      )
    )

    Sheet(name = repoDependencies.name).withRows(headerRow :: dataRows: _*)
  }
}

object Excel {
  def createWorkbook[A](data: List[A])(
      sheetExporter: SheetExporter[A]
  ): Workbook = Workbook().withSheets(data.map(sheetExporter.createSheet))

  def saveWorkbook(workbook: Workbook, path: String): Unit =
    workbook.saveAsXlsx(path)
}
