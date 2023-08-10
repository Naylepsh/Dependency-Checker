package scanning.application

import core.application.controller.Controller
import org.http4s.{ EntityDecoder, HttpRoutes, MediaType }
import org.http4s.headers.*
import org.http4s.dsl.Http4sDsl
import org.http4s.*
import cats.Monad
import cats.syntax.all.*
import scalatags.Text.all.*
import core.domain.project.ScanResult
import core.domain.project.ScanReport
import core.domain.severity.{ Severity, determineSeverity }
import core.domain.Time
import org.joda.time.DateTime
import core.domain.Time.DeltaUnit

object ScanningController:
  import ScanningViews.*

  def make[F[_]: Monad: Time](service: ScanReportService[F]): Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      def routes: HttpRoutes[F] = HttpRoutes.of[F]:
        case GET -> Root / "scan-report" / projectName / "latest" =>
          service
            .getLatestScan(projectName)
            .flatMap:
              case None =>
                layout(renderNoScanResult).pure
              case Some(scanReport) =>
                Time[F].currentDateTime.map: now =>
                  layout(renderScanResult(now, scanReport))
            .flatMap: html =>
              Ok(html.toString, `Content-Type`(MediaType.text.html))

private object ScanningViews:
  def layout(bodyContent: scalatags.Text.Modifier*) =
    html(
      head(
        script(src := "https://unpkg.com/htmx.org@1.9.4"),
        script(src := "https://unpkg.com/htmx.org/dist/ext/json-enc.js"),
        script(src := "https://cdn.tailwindcss.com")
      ),
      body(
        cls := "text-gray-200 bg-gray-900",
        bodyContent
      )
    )

  def renderScanResult(now: DateTime, scanResult: ScanReport) =
    div(
      cls := "container mx-auto my-10",
      h2(
        cls := "text-center font-semibold text-5xl",
        scanResult.projectName
      ),
      scanResult.dependenciesReports.map: group =>
        div(
          cls := "my-5",
          h3(cls := "text-2xl", s"> ${group.groupName}"),
          div(
            cls := "ml-5",
            group.items.sortBy(_.name.toLowerCase).map: dependencyReport =>
              val items = List(
                div(
                  cls := "flex justify-between",
                  div(
                    cls := "text-2xl",
                    dependencyReport.name
                  ),
                  renderSeverityBar(determineSeverity(now, dependencyReport))
                ),
                div(
                  cls := "my-3 flex justify-between",
                  p(
                    s"""Current version: ${dependencyReport.currentVersion.getOrElse(
                        "-"
                      )}"""
                  ),
                  p(s"Latest version: ${dependencyReport.latestVersion}"),
                  renderReleaseDate(now, dependencyReport.latestReleaseDate)
                )
              )

              div(
                cls := "my-3 p-3 bg-gray-800 text-gray-300 border-2 border-gray-700 grid grid-colrs-1 divide-y divide-gray-700",
                if dependencyReport.vulnerabilities.isEmpty
                then items
                else
                  items.appended(
                    renderVulnerabilities(dependencyReport.vulnerabilities)
                  )
              )
          )
        )
    )

  private def renderVulnerabilities(vulnerabilities: List[String]) =
    if vulnerabilities.isEmpty
    then div()
    else
      div(
        cls := "grid grid-cols-1 divide-y divide-gray-700 divide-dashed",
        vulnerabilities.map: vulnerability =>
          div(
            cls := "px-3 flex justify-between",
            p(cls      := "my-auto", vulnerability),
            button(cls := "bg-teal-500 m-1 py-1 px-3 text-gray-100", "Ignore")
          )
      )

  private def renderSeverityBar(severity: Severity) =
    val (color, barSize, leftoverSize) = severity match
      case Severity.Unknown => ("bg-slate-400", "w-full", "w-0")
      case Severity.None    => ("bg-green-500", "w-full", "w-0")
      case Severity.Low     => ("bg-lime-300", "w-3/4", "w-1/4")
      case Severity.Medium  => ("bg-yellow-500", "w-1/2", "w-1/2")
      case Severity.High    => ("bg-red-500", "w-1/4", "w-3/3")

    div(
      cls := "flex w-64 h-8 border-2 border-stone-500",
      div(cls := s"$color $barSize"),
      div(cls := leftoverSize)
    )

  private def renderReleaseDate(now: DateTime, releaseDate: Option[DateTime]) =
    val delta = releaseDate
      .map: date =>
        Time.Delta(date, now).show
      .getOrElse("-")
    p(s"Latest release: $delta ago")

  def renderNoScanResult = ???
