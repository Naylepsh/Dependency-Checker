package scanning.application

import core.application.controller.Controller
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*
import cats.Monad
import scalatags.Text.all.*
import core.domain.project.ScanResult

object ScanningController:
  def make[F[_]: Monad]: Controller[F] = new:
    def routes: HttpRoutes[F] = HttpRoutes.of[F]:
      case GET -> Root / "scan-report" / projectName / "latest" =>
        ???

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

  def renderScanResult(scanResult: ScanResult) = ???
  def renderNoScanResult = ???
