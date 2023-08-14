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
import core.domain.registry.DependencySource.{ TomlSource, TxtSource }

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
        case GET -> Root / "project" / projectName / "detailed" =>
          service
            .find(projectName)
            .flatMap:
              case None => ???
              case Some(project) => Ok(
                  renderProjectDetails(project).toString,
                  `Content-Type`(MediaType.text.html)
                )
        case GET -> Root / "project" / projectName / "short" =>
          service
            .find(projectName)
            .flatMap:
              case None => ???
              case Some(project) => Ok(
                  renderProjectShort(project).toString,
                  `Content-Type`(MediaType.text.html)
                )

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
        projects.map(renderProjectShort)
      )
    )

  def renderProjectShort(project: ProjectScanConfig) =
    // TODO: Add some animations when details unfold
    div(
      cls                 := "my-3 p-3 bg-gray-800 text-gray-300 border-2 border-gray-700 cursor-pointer",
      htmx.ajax.get       := s"/project/${project.name}/detailed",
      htmx.swap.attribute := htmx.swap.value.outerHTML,
      div(
        cls := "flex justify-between",
        p(cls := "text-2xl", project.name),
        div(
          cls := "ml-auto my-auto",
          a(
            cls                    := "bg-orange-500 m-1 py-2 px-3 text-gray-100 cursor-pointer",
            htmx.ajax.post         := s"/scan/${project.name}",
            htmx.trigger.attribute := htmx.trigger.value.click,
            htmx.swap.attribute    := htmx.swap.value.outerHTML,
            "Scan"
          ),
          a(
            cls  := "bg-teal-500 m-1 py-2 px-3 text-gray-100",
            href := s"/scan-report/${project.name}/latest",
            "Scan report"
          )
        )
      )
    )

  def renderProjectDetails(project: ProjectScanConfig) =
    div(
      cls                 := "my-3 p-3 bg-gray-800 text-gray-300 border-2 border-gray-700 cursor-pointer divide-y divide-gray-700",
      htmx.ajax.get       := s"/project/${project.name}/short",
      htmx.swap.attribute := htmx.swap.value.outerHTML,
      div(
        cls := "pb-3 flex justify-between",
        p(cls := "text-2xl", project.name),
        div(
          cls := "ml-auto my-auto",
          a(
            cls                    := "bg-orange-500 m-1 py-2 px-3 text-gray-100 cursor-pointer",
            htmx.ajax.post         := s"/scan/${project.name}",
            htmx.trigger.attribute := htmx.trigger.value.click,
            htmx.swap.attribute    := htmx.swap.value.outerHTML,
            "Scan"
          ),
          a(
            cls  := "bg-teal-500 m-1 py-2 px-3 text-gray-100",
            href := s"/scan-report/${project.name}/latest",
            "Scan report"
          )
        )
      ),
      div(
        cls := "pt-3",
        p(span(cls := "font-semibold", "Gitlab ID: "), project.id),
        p(span(cls := "font-semibold", "Target branch: "), project.branch),
        div(
          cls := "grid grid-cols-1 divide-y divide-gray-700 divide-dashed",
          span(cls := "font-semibold", "Sources:"),
          project
            .sources
            .map:
              case TxtSource(path) => path
              case TomlSource(path, group) =>
                val suffix = group
                  .map: group =>
                    s":$group"
                  .getOrElse("")
                s"$path:$suffix"
            .map: str =>
              p(cls := "pl-3", str)
        )
      )
    )
