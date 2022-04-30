package Dependencies

import spoiwo.model.{Row, Sheet, Font, Workbook, CellStyle}
import spoiwo.natures.xlsx.Model2XlsxConversions._

trait SheetExporter[A] {
  def createSheet(source: A): Sheet
}

class RepositoryDependenciesSheetExporter
    extends SheetExporter[RepositoryDependencies] {
  private val headerStyle = CellStyle(font = Font(bold = true))

  def createSheet(repoDependencies: RepositoryDependencies): Sheet = {
    val headerRow = Row(style = headerStyle)
      .withCellValues("Name", "Current Version", "Latest Version")
    val dataRows = repoDependencies.dependencies.map(d =>
      Row().withCellValues(
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
