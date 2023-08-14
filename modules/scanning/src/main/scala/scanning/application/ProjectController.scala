package scanning.application

import core.application.controller.Controller
import org.http4s.{ EntityDecoder, HttpRoutes, MediaType }
import org.http4s.headers.*
import org.http4s.dsl.Http4sDsl
import org.http4s.*
import cats.Monad
import cats.syntax.all.*
import scalatags.Text.all.*
import core.domain.registry.ProjectScanConfig

object htmx:
  // TODO: Move this to a dedicated module (/lib?)
  object ajax:
    val post = attr("hx-post")

  val trigger = attr("trigger")

object events:
  val click = "click"


object ProjectController:
  // TODO: Move this to a dedicated module
  // And mode ScanningViews' layout to a shared module (/lib?)
  import ProjectViews.*

  def make[F[_]: Monad](service: ProjectService[F]): Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      def routes: HttpRoutes[F] = HttpRoutes.of[F]:
        case GET -> Root / "project" =>
          service
            .all
            .map: projects =>
              layout(renderProjects(projects))
            .flatMap: html =>
              Ok(html.toString, `Content-Type`(MediaType.text.html))

object ProjectViews:
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

  def renderProjects(projects: List[ProjectScanConfig]) =
    div(
      cls := "container mx-auto my-10",
      h2(
        cls := "text-center font-semibold text-5xl",
        "Registered projects"
      ),
      div(
        cls := "my-5",
        projects.map(renderProject)
      )
    )

  def renderProject(project: ProjectScanConfig) =
    div(
      cls := "flex justify-between my-3 p-3 bg-gray-800 text-gray-300 border-2 border-gray-700",
      p(cls := "text-2xl", project.name),
      div(
        cls := "ml-auto my-auto",
        a(
          cls := "bg-orange-500 m-1 py-2 px-3 text-gray-100 cursor-pointer",
          htmx.ajax.post := s"/scan/${project.name}",
          htmx.trigger := events.click,
          "Scan"
        ),
        a(
          cls  := "bg-teal-500 m-1 py-2 px-3 text-gray-100",
          href := s"/scan-report/${project.name}/latest",
          "Scan report"
        )
      )
    )
