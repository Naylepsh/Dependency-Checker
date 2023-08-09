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
              case None             => renderNoScanResult
              case Some(scanReport) => renderScanResult(scanReport)
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
      body(bodyContent)
    )

  def renderScanResult(scanResult: ScanReport) =
    div(
      cls := "container",
      scanResult.dependenciesReports.map: group =>
        div(
          h1(group.groupName),
          div(
            group.items.map: dependencyReport =>
              div(
                dependencyReport.name,
                div(
                  p(dependencyReport.currentVersion.getOrElse("-")),
                  p(dependencyReport.latestVersion),
                  p(dependencyReport.latestReleaseDate.map(
                    _.toString
                  ).getOrElse("-")),
                  p(dependencyReport.vulnerabilities.length)
                )
              )
          )
        )
    )

  def renderNoScanResult = ???
