package core.domain

import org.joda.time.DateTime
import org.scalatest.*

import vulnerability.VulnerabilitySeverity
import dependency.{DependencyScanReport, DependencyVulnerability}
import severity.determineSeverity
import flatspec.*
import matchers.*

class severitySpec extends AnyFlatSpec with should.Matchers:
  import severitySpec.*
  import severity.*

  "Severity" should "be high when there's a vulnerability" in {
    determineSeverity(
      recentDate,
      recentDependency.copy(vulnerabilities =
        List(DependencyVulnerability("some-spooky-numbers-here", Some(VulnerabilitySeverity.Medium)))
      )
    ) shouldBe Severity.High
  }

  it should "be at least medium when it's possibly unmaintained" in {
    val latestVersionButOld =
      recentDependency.copy(latestReleaseDate = Some(oldDate))
    val sameMinorVersion =
      latestVersionButOld.copy(currentVersion = Some("1.23.0"))
    val sameMajorVersion =
      latestVersionButOld.copy(currentVersion = Some("1.2.34"))
    val unknownVersion      = latestVersionButOld.copy(currentVersion = None)
    val determineSeverityOf = determineSeverity.curried(recentDate)

    List(
      determineSeverityOf(latestVersionButOld),
      determineSeverityOf(sameMinorVersion),
      determineSeverityOf(sameMajorVersion),
      determineSeverityOf(unknownVersion)
    ) should contain noneOf (Severity.Unknown, Severity.None, Severity.Low)
  }

object severitySpec:
  val oldDate    = DateTime.parse("2019-04-02")
  val recentDate = DateTime.parse("2023-05-03")
  val recentDependency = DependencyScanReport(
    name = "recent-dependency",
    currentVersion = Some("1.23.4"),
    latestVersion = "1.23.4",
    currentVersionReleaseDate = None,
    latestReleaseDate = Some(DateTime.parse("2023-05-02")),
    vulnerabilities = List.empty
  )
