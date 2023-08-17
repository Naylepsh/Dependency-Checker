package scanning.application

import core.application.controller.Controller
import org.http4s.{ EntityDecoder, HttpRoutes, MediaType }
import org.http4s.headers.*
import org.http4s.dsl.Http4sDsl
import org.http4s.*
import cats.{ Monad, MonadError }
import cats.syntax.all.*
import scalatags.Text.all.*
import core.domain.project.ProjectScanConfig
import core.domain.dependency.DependencySource.{ TomlSource, TxtSource }
import org.legogroup.woof.{ *, given }
import cats.effect.kernel.Sync
import fs2.io.file.Files

object ProjectController:
  // TODO: Move this to a dedicated module
  // And mode ScanningViews' layout to a shared module (/lib?)
  import ProjectViews.*

  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]

  def make[F[_]: Monad: ThrowableMonadError: Logger: Sync: Files](
      service: ProjectService[F]
  ): Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      def routes: HttpRoutes[F] = HttpRoutes.of[F]:
        case request @ GET -> Root / "static" / path =>
          // TODO: Move this to a dedicated controller
          val p = fs2.io.file.Path(s"./static/$path")
          StaticFile.fromPath(p, Some(request)).getOrElseF(NotFound())
        case GET -> Root / "project" =>
          service
            .all
            .map: projects =>
              views.layout(renderProjects(projects))
            .flatMap: html =>
              Ok(html.toString, `Content-Type`(MediaType.text.html))
            .handleErrorWith: error =>
              Logger[F].error(error.toString)
                *> InternalServerError("Oops, something went wrong")
        case GET -> Root / "project" / "form" =>
          Ok(
            views.layout(renderProjectForm).toString,
            `Content-Type`(MediaType.text.html)
          )
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

  def renderProjectShort(config: ProjectScanConfig) =
    // TODO: Add some animations when details unfold
    div(
      id  := config.project.name,
      cls := "my-3 p-3 bg-gray-800 text-gray-300 border-2 border-gray-700 cursor-pointer",
      div(
        cls := "flex justify-between",
        p(
          cls                   := "grow text-2xl",
          htmx.ajax.get         := s"/project/${config.project.name}/detailed",
          htmx.swap.attribute   := htmx.swap.value.outerHTML,
          htmx.target.attribute := s"#${config.project.name}",
          config.project.name
        ),
        div(
          cls := "my-auto",
          a(
            cls                    := "bg-orange-500 m-1 py-2 px-3 text-gray-100 cursor-pointer",
            htmx.ajax.post         := s"/scan/${config.project.name}",
            htmx.trigger.attribute := htmx.trigger.value.click,
            htmx.swap.attribute    := htmx.swap.value.outerHTML,
            "Scan"
          ),
          a(
            cls  := "bg-teal-500 m-1 py-2 px-3 text-gray-100",
            href := s"/scan-report/${config.project.name}/latest",
            "Scan report"
          )
        )
      )
    )

  def renderProjectDetails(config: ProjectScanConfig) =
    div(
      id  := config.project.name,
      cls := "my-3 p-3 bg-gray-800 text-gray-300 border-2 border-gray-700 cursor-pointer divide-y divide-gray-700",
      div(
        cls := "pb-3 flex justify-between",
        div(
          cls                   := "grow text-2xl",
          htmx.ajax.get         := s"/project/${config.project.name}/short",
          htmx.swap.attribute   := htmx.swap.value.outerHTML,
          htmx.target.attribute := s"#${config.project.name}",
          config.project.name
        ),
        div(
          cls := "my-auto",
          a(
            cls                    := "bg-orange-500 m-1 py-2 px-3 text-gray-100 cursor-pointer",
            htmx.ajax.post         := s"/scan/${config.project.name}",
            htmx.trigger.attribute := htmx.trigger.value.click,
            htmx.swap.attribute    := htmx.swap.value.outerHTML,
            "Scan"
          ),
          a(
            cls  := "bg-teal-500 m-1 py-2 px-3 text-gray-100",
            href := s"/scan-report/${config.project.name}/latest",
            "Scan report"
          )
        )
      ),
      div(
        cls := "pt-3",
        p(span(cls := "font-semibold", "Gitlab ID: "), config.project.id),
        p(span(cls := "font-semibold", "Target branch: "), config.branch),
        div(
          cls := "grid grid-cols-1 divide-y divide-gray-700 divide-dashed",
          span(cls := "font-semibold", "Sources:"),
          config
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

  val renderProjectForm =
    div(
      cls := "container mx-auto my-10",
      h2(
        cls := "text-center font-semibold text-3xl",
        "Create a new project config"
      ),
      div(
        cls := "w-full max-w-md mx-auto",
        form(
          cls            := "text-sm font-bold",
          htmx.ajax.post := "/project",
          attr("hx-ext") := "json-enc",
          div(
            cls := "mb-4",
            formLabel("name", "Name"),
            formInput("name")
          ),
          div(
            cls := "mb-4",
            formLabel("gitlabId", "Gitlab ID"),
            formInput("gitlabId")
          ),
          div(
            cls := "mb-4",
            formLabel("branch", "Branch"),
            formInput("branch")
          ),
          div(
            cls := "mb-4",
            formLabel("sources", "Sources"),
            div(
              cls := "grid grid-cols-1 divide-y divide-gray-700 divide-dashed border border-2 border-gray-400",
              txtSourceInput,
              tomlSourceInput
            )
          ),
          button(cls := "w-full bg-teal-500 py-2 px-3", "Submit")
        )
      )
    )

  private def sourceInput(i: Int) =
    div(
      cls := "flex",
      formInput(s"sources[path]"),
      formInput(s"sources[group]"),
      button(
        cls     := "px-3 bg-teal-500",
        onclick := "removeParent(this)",
        `type`  := "button",
        "X"
      )
    )

  private def txtSourceInput =
    div(
      cls := "p-3 form-group",
      div(
        cls := "flex",
        h4(cls := "w-full mb-3", "TXT source"),
        button(
          cls     := "px-3 bg-teal-500",
          onclick := "removeClosest(this, '.form-group')",
          `type`  := "button",
          "X"
        )
      ),
      formLabel("sources[path]", "Path"),
      formInput(s"sources[path]")
    )

  private def tomlSourceInput =
    div(
      cls := "p-3 form-group",
      div(
        cls := "flex",
        h4(cls := "w-full mb-3", "TOML source"),
        button(
          cls     := "px-3 bg-teal-500",
          onclick := "removeClosest(this, '.form-group')",
          `type`  := "button",
          "X"
        )
      ),
      div(
        formLabel("sources[path]", "Path"),
        formInput(s"sources[path]")
      ),
      div(
        formLabel("sources[group]", "Group"),
        formInput(s"sources[group]")
      )
    )

  private def formLabel(elementName: String, labelText: String) =
    label(
      cls   := "block font-bold",
      `for` := elementName,
      labelText
    )

  private def formInput(inputName: String) =
    input(
      cls  := "shadow appearance-none border w-full py-2 px-3 text-gray-300 leading-tight focus:outline-none focus:shadow-outline focus:border-teal-200 bg-gray-900",
      name := inputName
    )
