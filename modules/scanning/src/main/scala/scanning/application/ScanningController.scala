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

object ScanningController:
  import ScanningViews.*

  def make[F[_]: Monad](service: ScanReportService[F]): Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      def routes: HttpRoutes[F] = HttpRoutes.of[F]:
        case GET -> Root / "scan-report" / projectName / "latest" =>
          service
            .getLatestScan(projectName)
            .map:
              case None             => layout(renderNoScanResult)
              case Some(scanReport) => layout(renderScanResult(scanReport))
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
        cls := "text-stone-50 bg-zinc-900",
        bodyContent
      )
    )

  def renderScanResult(scanResult: ScanReport) =
    div(
      cls := "container mx-auto my-10",
      h2(
        cls := "text-center text-rose-900 font-semibold text-5xl",
        scanResult.projectName
      ),
      scanResult.dependenciesReports.map: group =>
        div(
          cls := "my-5",
          h3(cls := "text-2xl", s"> ${group.groupName}"),
          div(
            cls := "ml-5",
            group.items.map: dependencyReport =>
              val items = List(
                div(
                  cls := "text-2xl",
                  dependencyReport.name
                ),
                div(
                  cls := "my-3 flex justify-between",
                  p(
                    s"""Current version: ${dependencyReport.currentVersion.getOrElse(
                        "-"
                      )}"""
                  ),
                  p(s"Latest version: ${dependencyReport.latestVersion}"),
                  p(s"""Latest release date: ${dependencyReport.latestReleaseDate.map(
                      _.toString
                    ).getOrElse("-")}""")
                )
              )
              div(
                cls := "my-3 p-3 border-2 border-rose-900 grid grid-colrs-1 divide-y divide-rose-900",
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
        cls := "grid grid-cols-1 divide-y divide-rose-900 divide-dashed",
        vulnerabilities.map: vulnerability =>
          p(cls := "px-3", vulnerability)
      )

  def renderNoScanResult = ???
