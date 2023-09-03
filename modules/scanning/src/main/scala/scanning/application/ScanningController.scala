package scanning.application

import cats.syntax.all.*
import cats.{ Monad, MonadThrow }
import cats.data.{ Validated, ValidatedNel }
import core.application.controller.Controller
import core.domain.severity
import core.domain.Time
import core.domain.Time.DeltaUnit
import core.domain.project.{
  ProjectScanConfigRepository,
  ScanReport,
  ScanResult
}
import core.domain.severity.{ Severity, determineSeverity }
import core.domain.task.TaskProcessor
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.server.Router
import org.joda.time.DateTime
import org.legogroup.woof.{ *, given }
import scalatags.Text.all.*
import scanning.application.services.ScanningService
import org.http4s.dsl.impl.OptionalValidatingQueryParamDecoderMatcher
import core.domain.dependency.DependencyReport
import cats.data.NonEmptyList

object ScanningController:
  import ScanningViews.*

  enum SortDirection:
    case Asc, Desc
  object SortDirection:
    given QueryParamDecoder[SortDirection] = QueryParamDecoder[String].emap:
      case "asc"  => SortDirection.Asc.asRight
      case "desc" => SortDirection.Desc.asRight
      case other =>
        val message = s"$other is not a valid sort direction"
        ParseFailure(message, message).asLeft

  enum SortByProperty:
    case Name, Severity
  object SortByProperty:
    given QueryParamDecoder[SortByProperty] = QueryParamDecoder[String].emap:
      case "name"     => SortByProperty.Name.asRight
      case "severity" => SortByProperty.Severity.asRight
      case other =>
        val message = s"$other is not a valid property to sort by"
        ParseFailure(message, message).asLeft

  object SortByPropertyQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[SortByProperty](
        "sort-by"
      )
  object SortDirectionPropertyQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[SortDirection](
        "sort-dir"
      )

  def make[F[_]: MonadThrow: Time: Logger](
      service: ScanningService[F],
      repository: ProjectScanConfigRepository[F],
      taskProcessor: TaskProcessor[F]
  ): Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F]:
        case GET -> Root / projectName / "latest"
            :? SortByPropertyQueryParamMatcher(maybeSortByProperty)
            +& SortDirectionPropertyQueryParamMatcher(maybeSortDirection) =>
          Time[F].currentDateTime.flatMap: now =>
            (maybeSortByProperty, maybeSortDirection)
              .tupled
              .map: (validatedProperty, validatedDirection) =>
                (validatedProperty, validatedDirection)
                  .tupled
                  .map:
                    case (SortByProperty.Name, SortDirection.Asc) =>
                      DependencyReport.compareByNameAsc
                    case (SortByProperty.Name, SortDirection.Desc) =>
                      DependencyReport.compareByNameDesc
                    case (SortByProperty.Severity, SortDirection.Asc) =>
                      DependencyReport.compareBySeverityAsc(now)
                    case (SortByProperty.Severity, SortDirection.Desc) =>
                      DependencyReport.compareBySeverityDesc(now)
              .getOrElse(DependencyReport.compareByNameAsc.validNel)
              .fold(
                errors => BadRequest(errors.toString),
                compare =>
                  service
                    .getLatestScan(projectName)
                    .map:
                      case None =>
                        views.layout(renderNoScanResult)
                      case Some(scanReport) =>
                        val report = ScanReport.sortGroups(compare, scanReport)
                        views.layout(renderScanResult(now, report))
                    .flatMap: html =>
                      Ok(html.toString, `Content-Type`(MediaType.text.html))
                    .handleErrorWith: error =>
                      Logger[F].error(error.toString)
                        *> InternalServerError("Oops, something went wrong")
              )

        case POST -> Root / "all" =>
          repository.all.flatMap: configs =>
            configs
              .filter(_.enabled)
              .traverse: config =>
                taskProcessor.add(service.scan(config))
              .flatMap: _ =>
                Ok(
                  renderAllScansScheduledButton.toString,
                  `Content-Type`(MediaType.text.html)
                )

        case POST -> Root / projectName =>
          repository.all.flatMap: configs =>
            configs
              .find: config =>
                config.project.name == projectName
              .fold(NotFound(s"$projectName does not exist")): project =>
                taskProcessor.add(service.scan(project))
                  *> Ok(
                    renderScanScheduledButton.toString,
                    `Content-Type`(MediaType.text.html)
                  )

      val routes: HttpRoutes[F] = Router("scan" -> httpRoutes)

private object ScanningViews:
  def renderScanResult(now: DateTime, scanResult: ScanReport) =
    div(
      cls := "container mx-auto my-10",
      h2(
        cls := "text-center font-semibold text-5xl",
        scanResult.projectName
      ),
      div(
        id  := "actions",
        cls := "flex w-100 ml-auto",
        div(
          id  := "sorting",
          cls := "ml-auto",
          span(cls := "mr-1", "Sort by:"),
          button(
            cls := "bg-gray-800 border-2 border-r-0 border-gray-700 p-1",
            "Name"
          ),
          button(
            cls := "bg-gray-800 border-2 border-gray-700 p-1",
            "Severity"
          )
        )
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

  def renderNoScanResult =
    // TODO: Add prettier view
    div("This project hasn't been scanned yet")

  val renderScanScheduledButton =
    a(
      cls := "bg-gray-400 m-1 py-2 px-3 text-gray-100 cursor-pointer animate-pulse",
      "Scan scheduled"
    )

  // TODO: project controller should hx-swap: none and add classes: animate-pulse and bg-gray-400 after successful request
  val renderAllScansScheduledButton =
    a(
      cls := "block w-full my-3 p-4 bg-gray-400 text-gray-100 border-2 border-gray-700 cursor-pointer text-center animate-pulse",
      "Scans scheduled"
    )
