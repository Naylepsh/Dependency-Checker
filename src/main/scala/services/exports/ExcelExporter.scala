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

  enum VersionDifference:
    case Patch, Minor, Major

  def calculateVersionDifference(
      a: String,
      b: String
  ): Try[Option[VersionDifference]] = {
    if (a == b)
      Success(None)
    else {
      for {
        (aSymbol, aMajor, aMinor, aPatch) <- extractVersion(a)
        (_, bMajor, bMinor, bPatch) <- extractVersion(b)
      } yield {
        if (aMajor != bMajor)
          Some(VersionDifference.Major)
        else if (aMajor == bMajor && aSymbol == Some("^"))
          None
        else if (aMinor != bMinor)
          Some(VersionDifference.Minor)
        else if (aMinor == bMinor && aSymbol == Some("~"))
          None
        else if (aPatch != bPatch)
          Some(VersionDifference.Patch)
        else
          None
      }
    }
  }

  private def extractVersion(
      text: String
  ): Try[(Option[String], Option[String], Option[String], Option[String])] = {
    versionPattern
      .findFirstMatchIn(text)
      .map(matches => {
        val symbol = Option(matches.group(1))
        val major = Option(matches.group(2))
        val minor = Option(matches.group(3))
        val patch = Option(matches.group(4))

        (symbol, major, minor, patch)
      }) match {
      case Some(version) => Success(version)
      case None =>
        Failure(RuntimeException(s"Could not parse the version from $text"))
    }
  }

  private val versionPattern = "([~^])*([0-9])*.([0-9a-z])*.?([0-9a-z])*".r
}
