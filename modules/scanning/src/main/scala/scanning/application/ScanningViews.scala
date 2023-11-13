package scanning.application

import cats.syntax.all.*
import core.domain.Time
import core.domain.Time.DeltaUnit
import core.domain.dependency.{ DependencyScanReport, DependencyVulnerability }
import core.domain.project.{ ProjectVulnerability, ScanReport }
import core.domain.severity.{ Severity, determineSeverity }
import org.joda.time.DateTime
import scalatags.Text.all.*
import core.domain.update.UpdateDependency
import io.circe.syntax.*
import io.circe.generic.auto.*
import scanning.domain.ScanSummary
import scalatags.Text.TypedTag
import scanning.domain.DependencySummary
import core.domain.Grouped

object ScanningViews:
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

  def renderScanSummary(
      now: DateTime,
      scanResult: ScanSummary,
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
      scanResult.dependencySummaries.map: group =>
        renderScanSummaryGroup(now, group, scanResult.projectName)
    )

  private def renderScanSummaryGroup(
      now: DateTime,
      group: Grouped[DependencySummary],
      projectName: String
  ) =
    div(
      cls := "my-5",
      h3(cls := "text-2xl", s"> ${group.groupName}"),
      div(
        cls := "ml-5",
        group.items.map: dependencySummary =>
          var actions = List.empty[TypedTag[String]]
          dependencySummary
            .scanReport
            .currentVersion
            .foreach: currentVersion =>
              if dependencySummary.canBeUpdated then
                actions = button(
                  cls            := "bg-blue-500 ml-3 px-2 py-1 disabled:opacity-75 disabled:bg-gray-700",
                  htmx.ajax.post := "/api/update",
                  htmx.extraValues.vals := htmx.extraValues.value.vals(
                    UpdateDependency(
                      projectName,
                      dependencySummary.scanReport.name,
                      group.groupName,
                      currentVersion,
                      dependencySummary.scanReport.latestVersion
                    )
                  ),
                  attr("hx-ext")             := "json-enc",
                  htmx.hyperscript.attribute := "on click toggle @disabled",
                  "Update"
                ) :: actions

          var summaryItems = List(
            div(
              cls := "flex justify-between",
              div(cls := "text-2xl", dependencySummary.scanReport.name),
              div(
                cls := "flex",
                actions,
                div(
                  cls := "ml-3",
                  renderSeverityBar(determineSeverity(
                    now,
                    dependencySummary.scanReport
                  ))
                )
              )
            ),
            div(
              cls := "mt-3 pt-3 flex justify-between",
              p(
                s"""Current version: ${dependencySummary.scanReport.currentVersion.getOrElse(
                    "-"
                  )}"""
              ),
              p(
                s"Latest version: ${dependencySummary.scanReport.latestVersion}"
              ),
              renderReleaseDate(
                now,
                dependencySummary.scanReport.latestReleaseDate
              )
            )
          )

          if !dependencySummary.scanReport.vulnerabilities.isEmpty then
            summaryItems = summaryItems.appended(
              renderVulnerabilities(
                dependencySummary.scanReport.vulnerabilities
              )
            )

          div(
            cls := "mt-3 p-3 bg-gray-800 text-gray-300 border-2 border-gray-700 grid grid-colrs-1 divide-y divide-gray-700",
            summaryItems
          )
      )
    )

  private def inferLink(vulnerability: String) =
    if vulnerability.startsWith("GHSA-") then
      s"https://github.com/advisories/$vulnerability".some
    else if vulnerability.startsWith("PYSEC-") then
      s"https://osv.dev/vulnerability/$vulnerability".some
    else None

  private def renderVulnerabilities(
      vulnerabilities: List[DependencyVulnerability]
  ) =
    if vulnerabilities.isEmpty
    then div()
    else
      div(
        cls := "mt-3 grid grid-cols-1 divide-y divide-gray-700 divide-dashed",
        vulnerabilities.map: vulnerability =>
          val nameElem = inferLink(vulnerability.name) match
            case Some(link) =>
              a(
                cls  := "my-auto text-blue-300",
                href := link,
                vulnerability.name
              )
            case None =>
              p(cls := "my-auto", vulnerability.name)

          val severityElem =
            p(
              cls := "my-auto",
              s"${vulnerability.severity.map(_.show).getOrElse("Unknown")} severity"
            )

          div(
            cls := "px-3 py-2",
            nameElem,
            severityElem
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
      projectsVulnerabilities: List[ProjectVulnerability],
      daysSince: Option[Int]
  ) =
    val coreInputModifiers = List(
      cls    := "text-base mx-2 bg-gray-900 w-12",
      name   := "days-since",
      `type` := "number",
      min    := 0
    )
    val inputElem = daysSince match
      case Some(days) => input(coreInputModifiers, value := days)
      case None       => input(coreInputModifiers, placeholder := 1)

    div(
      cls := "container mx-auto my-10",
      div(
        form(
          cls    := "text-xl",
          action := "/scan/vulnerability",
          p(cls := "inline-block", "Show vulnerabilities found within"),
          inputElem,
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
