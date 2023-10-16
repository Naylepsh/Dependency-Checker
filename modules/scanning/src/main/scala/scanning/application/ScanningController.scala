package scanning.application

import cats.syntax.all.*
import cats.Monad
import cats.data.{ Validated, ValidatedNel }
import core.controller.Controller
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
import org.http4s.dsl.impl.ValidatingQueryParamDecoderMatcher
import core.domain.dependency.DependencyReport
import cats.data.NonEmptyList
import cats.data.Validated.Valid
import core.domain.project.ProjectVulnerability
import cats.data.Validated.Invalid

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

object ScanningController:
  import ScanningViews.*
  object SortByPropertyQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[SortByProperty](
        "sort-by"
      )
  object SortDirectionPropertyQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[SortDirection](
        "sort-dir"
      )
  object DaysSinceQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[Int]("days-since")

  private def makeComparator(
      now: DateTime,
      property: SortByProperty,
      direction: SortDirection
  ) =
    (property, direction) match
      case (SortByProperty.Name, SortDirection.Asc) =>
        DependencyReport.compareByNameAsc
      case (SortByProperty.Name, SortDirection.Desc) =>
        DependencyReport.compareByNameDesc
      case (SortByProperty.Severity, SortDirection.Asc) =>
        DependencyReport.compareBySeverityAsc(now)
      case (SortByProperty.Severity, SortDirection.Desc) =>
        DependencyReport.compareBySeverityDesc(now)

  def make[F[_]: Monad: Time: Logger](
      service: ScanningService[F],
      repository: ProjectScanConfigRepository[F],
      taskProcessor: TaskProcessor[F]
  ): Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F]:
        case GET -> Root / "project" / projectName / "latest"
            :? SortByPropertyQueryParamMatcher(maybeSortByProperty)
            +& SortDirectionPropertyQueryParamMatcher(maybeSortDirection) =>
          Time[F].currentDateTime.flatMap: now =>
            (maybeSortByProperty, maybeSortDirection)
              .tupled
              .map: (validatedProperty, validatedDirection) =>
                (validatedProperty, validatedDirection).tupled
              .getOrElse(Valid(SortByProperty.Name, SortDirection.Asc))
              .fold(
                errors => BadRequest(errors.toString),
                // get project's latest scan
                (sortByProperty, sortDirection) =>
                  service
                    .getLatestScan(projectName)
                    .map:
                      case None =>
                        views.layout(renderNoScanResult)
                      case Some(scanReport) =>
                        val compare =
                          makeComparator(now, sortByProperty, sortDirection)
                        val report = ScanReport.sortGroups(compare, scanReport)
                        views.layout(renderScanResult(
                          now,
                          report,
                          sortByProperty,
                          sortDirection
                        ))
                    .flatMap: html =>
                      Ok(html.toString, `Content-Type`(MediaType.text.html))
              )

        case POST -> Root / "project" / "all" =>
          repository
            .all
            .flatMap: configs =>
              configs
                .filter(_.enabled)
                .traverse: config =>
                  taskProcessor.add(service.scan(config))
            .flatMap: _ =>
              taskProcessor.add(
                service.obtainUnknownSeveritiesOfVulnerabilities
              )
            .flatMap: _ =>
              Ok(
                renderAllScansScheduledButton.toString,
                `Content-Type`(MediaType.text.html)
              )

        case POST -> Root / "project" / projectName =>
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

        case GET -> Root / "vulnerability"
            :? DaysSinceQueryParamMatcher(daysSince) =>
          daysSince match
            case None =>
              val html =
                views.layout(renderProjectsVulnerabilitiesView(List.empty))
              Ok(html.toString, `Content-Type`(MediaType.text.html))
            case Some(Valid(daysSince)) =>
              for
                now <- Time[F].currentDateTime
                time = now.minusDays(daysSince)
                vulnerabilities <- service.getVulnerabilitiesSince(time)
                html =
                  views.layout(
                    renderProjectsVulnerabilitiesView(vulnerabilities)
                  )
                result <- Ok(html.toString, `Content-Type`(MediaType.text.html))
              yield result
            case Some(Invalid(errors)) =>
              BadRequest(errors.toString)

      val routes: HttpRoutes[F] = Router("scan" -> httpRoutes)

private object ScanningViews:
  private def renderSortByNameAction(
      projectName: String,
      sortedByProperty: SortByProperty,
      sortedInDirection: SortDirection
  ) =
    val background = sortedByProperty match
      case SortByProperty.Name => "bg-gray-900"
      case _                   => "bg-gray-800"
    val (link, maybeIcon) = (sortedByProperty, sortedInDirection) match
      case (SortByProperty.Name, SortDirection.Asc) =>
        (
          s"/scan/project/$projectName/latest?sort-by=name&sort-dir=desc",
          i(cls := "fa fa-solid fa-up-long ml-1").some
        )
      case (SortByProperty.Name, SortDirection.Desc) =>
        (
          s"/scan/project/$projectName/latest?sort-by=name&sort-dir=asc",
          i(cls := "fa fa-solid fa-down-long ml-1").some
        )
      case _ =>
        (s"/scan/project/$projectName/latest?sort-by=name&sort-dir=asc", None)

    a(
      cls  := s"$background border-2 border-r-0 border-gray-700 p-1",
      href := link,
      "Name",
      maybeIcon.getOrElse(i())
    )

  private def renderSortBySeverityAction(
      projectName: String,
      sortedByProperty: SortByProperty,
      sortedInDirection: SortDirection
  ) =
    val background = sortedByProperty match
      case SortByProperty.Severity => "bg-gray-900"
      case _                       => "bg-gray-800"
    val (link, maybeIcon) = (sortedByProperty, sortedInDirection) match
      case (SortByProperty.Severity, SortDirection.Asc) =>
        (
          s"/scan/project/$projectName/latest?sort-by=severity&sort-dir=desc",
          i(cls := "fa fa-solid fa-up-long ml-1").some
        )
      case (SortByProperty.Severity, SortDirection.Desc) =>
        (
          s"/scan/project/$projectName/latest?sort-by=severity&sort-dir=asc",
          i(cls := "fa fa-solid fa-down-long ml-1").some
        )
      case _ =>
        (
          s"/scan/project/$projectName/latest?sort-by=severity&sort-dir=asc",
          None
        )

    a(
      cls  := s"$background border-2 border-gray-700 p-1",
      href := link,
      "Severity",
      maybeIcon.getOrElse(i())
    )

  def renderScanResult(
      now: DateTime,
      scanResult: ScanReport,
      sortedByProperty: SortByProperty,
      sortedInDirection: SortDirection
  ) =
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
          renderSortByNameAction(
            scanResult.projectName,
            sortedByProperty,
            sortedInDirection
          ),
          renderSortBySeverityAction(
            scanResult.projectName,
            sortedByProperty,
            sortedInDirection
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

  private def inferLink(vulnerability: String) =
    if vulnerability.startsWith("GHSA-") then
      s"https://github.com/advisories/$vulnerability".some
    else if vulnerability.startsWith("PYSEC-") then
      s"https://osv.dev/vulnerability/$vulnerability".some
    else None

  private def renderVulnerabilities(vulnerabilities: List[String]) =
    if vulnerabilities.isEmpty
    then div()
    else
      div(
        cls := "grid grid-cols-1 divide-y divide-gray-700 divide-dashed",
        vulnerabilities.map: vulnerability =>
          val elem = inferLink(vulnerability) match
            case Some(link) =>
              a(cls := "my-auto text-blue-300", href := link, vulnerability)
            case None =>
              p(cls := "my-auto", vulnerability)

          div(
            cls := "px-3 py-2",
            elem
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

  def renderProjectsVulnerabilitiesView(
      projectsVulnerabilities: List[ProjectVulnerability]
  ) =
    div(
      cls := "container mx-auto my-10",
      div(
        form(
          cls    := "text-xl",
          action := "/scan/vulnerability",
          p(cls := "inline-block", "Show vulnerabilities found within"),
          input(
            cls         := "text-base mx-2 bg-gray-900 w-12",
            name        := "days-since",
            placeholder := 1,
            `type`      := "number",
            min         := 0
          ),
          label("days")
        )
      ),
      if projectsVulnerabilities.isEmpty
      then div()
      else
        table(
          cls := "w-full text-left",
          thead(
            cls := "border-b font-medium border-neutral-500",
            tr(
              th(cls := "px-6 py-4", "Project"),
              th(cls := "px-6 py-4", "Dependency name"),
              th(cls := "px-6 py-4", "Dependency version"),
              th(cls := "px-6 py-4", "Vulnerability name"),
              th(cls := "px-6 py-4", "Vulnerability severity")
            )
          ),
          tbody(
            cls := "border-b border-neutral-500",
            projectsVulnerabilities.zipWithIndex.map:
              (projectVulnerability, i) =>
                val vulnerabilityElem =
                  inferLink(projectVulnerability.vulnerabilityName) match
                    case Some(link) =>
                      a(
                        cls  := "text-blue-300",
                        href := link,
                        projectVulnerability.vulnerabilityName
                      )
                    case None =>
                      p(projectVulnerability.vulnerabilityName)
                val bg = if i % 2 == 0 then "bg-gray-800" else "bg-gray-900"
                tr(
                  cls := s"border-b border-neutral-500 $bg",
                  td(
                    cls := "whitespace-nowrap px-6 py-4",
                    projectVulnerability.projectName
                  ),
                  td(
                    cls := "whitespace-nowrap px-6 py-4",
                    projectVulnerability.dependencyName
                  ),
                  td(
                    cls := "whitespace-nowrap px-6 py-4",
                    projectVulnerability.dependencyVersion.getOrElse("-")
                  ),
                  td(
                    cls := "whitespace-nowrap px-6 py-4",
                    vulnerabilityElem
                  ),
                  td(
                    cls := "whitespace-nowrap px-6 py-4",
                    projectVulnerability
                      .vulnerabilitySeverity
                      .map(_.toString)
                      .getOrElse("-")
                  )
                )
          )
        )
    )
