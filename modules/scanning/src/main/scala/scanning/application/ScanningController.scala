package scanning.application

import cats.Monad
import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ NonEmptyList, Validated, ValidatedNel }
import cats.syntax.all.*
import core.controller.Controller
import core.domain.Time.DeltaUnit
import core.domain.dependency.{
  DependencyReport,
  DependencyScanReport,
  DependencyVulnerability
}
import core.domain.project.*
import core.domain.severity.{ Severity, determineSeverity }
import core.domain.{ Time, severity }
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.{
  OptionalValidatingQueryParamDecoderMatcher,
  ValidatingQueryParamDecoderMatcher
}
import org.http4s.headers.*
import org.http4s.server.Router
import org.joda.time.DateTime
import scalatags.Text.all.*
import scanning.application.services.ScanningService

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
        DependencyScanReport.compareByNameAsc
      case (SortByProperty.Name, SortDirection.Desc) =>
        DependencyScanReport.compareByNameDesc
      case (SortByProperty.Severity, SortDirection.Asc) =>
        DependencyScanReport.compareBySeverityAsc(now)
      case (SortByProperty.Severity, SortDirection.Desc) =>
        DependencyScanReport.compareBySeverityDesc(now)

  def make[F[_]: Monad: Time](
      service: ScanningService[F],
      repository: ProjectScanConfigRepository[F]
  ): Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F]:
        case GET -> Root / "project" / projectName / "latest"
            :? SortByPropertyQueryParamMatcher(maybeSortByProperty)
            +& SortDirectionPropertyQueryParamMatcher(maybeSortDirection) =>
          val title = projectName.some
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
                  val compare =
                    makeComparator(now, sortByProperty, sortDirection)
                  service
                    .getLatestScan(projectName, compare)
                    .map:
                      case None =>
                        views.layout(title, renderNoScanResult)
                      case Some(report) =>
                        views.layout(
                          title,
                          renderScanSummary(
                            now,
                            report,
                            sortByProperty,
                            sortDirection
                          )
                        )
                    .flatMap: html =>
                      Ok(html.toString, `Content-Type`(MediaType.text.html))
              )

        case POST -> Root / "project" / "all" =>
          service.scanAll.flatMap: _ =>
            Ok(
              renderAllScansScheduledButton.toString,
              `Content-Type`(MediaType.text.html)
            )

        case POST -> Root / "project" / projectName =>
          service.scanSingle(projectName).flatMap:
            case None => NotFound(s"$projectName does not exist")
            case Some(_) => Ok(
                renderScanScheduledButton.toString,
                `Content-Type`(MediaType.text.html)
              )

        case GET -> Root / "vulnerability"
            :? DaysSinceQueryParamMatcher(daysSince) =>
          val title = "Vulnerabilities".some
          daysSince match
            case None =>
              val html =
                views.layout(
                  title,
                  renderProjectsVulnerabilitiesView(List.empty, None)
                )
              Ok(html.toString, `Content-Type`(MediaType.text.html))
            case Some(Valid(daysSince)) =>
              for
                now <- Time[F].currentDateTime
                time = now.minusDays(daysSince)
                vulnerabilities <- service.getVulnerabilitiesSince(time)
                html =
                  views.layout(
                    title,
                    renderProjectsVulnerabilitiesView(
                      vulnerabilities,
                      daysSince.some
                    )
                  )
                result <- Ok(html.toString, `Content-Type`(MediaType.text.html))
              yield result
            case Some(Invalid(errors)) =>
              BadRequest(errors.toString)

      val routes: HttpRoutes[F] = Router("scan" -> httpRoutes)
