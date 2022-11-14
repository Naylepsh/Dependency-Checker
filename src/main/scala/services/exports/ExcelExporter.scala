package services.exports

import spoiwo.model.{Row, Sheet, Font, Workbook, CellStyle}
import spoiwo.natures.xlsx.Model2XlsxConversions._
import org.apache.poi.ss.usermodel.Cell
import scala.util.Success
import scala.util.Failure
import spoiwo.model.Color
import spoiwo.model.enums.CellFill
import cats._
import cats.implicits._
import domain.project.ExportProjectDependencies
import domain.dependency.DependencyReport
import domain.semver._
import scala.util.Try

object ExcelExporter {
  def make[F[_]: Applicative, A](
      toSheet: A => Sheet,
      path: String
  ): Exporter[F, A] = new Exporter[F, A] {

    override def exportData(data: List[A]): F[Unit] = {
      val workbook = Workbook().withSheets(data.map(toSheet))
      workbook.saveAsXlsx(path).pure
    }
  }

  object dependencies {
    def toSheet(repoDependencies: ExportProjectDependencies): Sheet = {
      val headerRow = Row(style = headerStyle)
        .withCellValues(
          "Name",
          "Current Version",
          "Latest Version",
          "Vulnerabilities",
          "Notes"
        )
      val dataRows = repoDependencies.dependenciesReports.map(d =>
        Row(style = chooseStyle(d)).withCellValues(
          d.name,
          d.currentVersion.getOrElse(""),
          d.latestVersion,
          d.vulnerabilities.mkString(",\n"),
          d.notes.getOrElse("")
        )
      )

      Sheet(name = repoDependencies.project.name)
        .withRows(headerRow :: dataRows: _*)
    }

    private val headerStyle = CellStyle(font = Font(bold = true))

    private def chooseStyle(dependency: DependencyReport): CellStyle = {
      val versionDifference = for {
        current <- dependency.currentVersion
      } yield calculateVersionDifference(current, dependency.latestVersion)

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

  }

}
