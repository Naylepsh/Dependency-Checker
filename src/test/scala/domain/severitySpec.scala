package domain

import domain.dependency.DependencyReport
import domain.severity.determineSeverity
import org.joda.time.DateTime
import org.scalatest.*

import flatspec.*
import matchers.*

class severitySpec extends AnyFlatSpec with should.Matchers:
  import severitySpec.*
  import severity.*

  "Severity" should "be high when there's a vulnerability" in {
    determineSeverity(recentDate)(recentDependency.copy(vulnerabilities =
      List("some-spooky-numbers-here")
    )) shouldBe Severity.High
  }

  it should "be at least medium when it's possibly unmaintained" in {
    val latestVersionButOld =
      recentDependency.copy(latestReleaseDate = Some(oldDate))
    val olderVersion        = latestVersionButOld.copy(currentVersion = Some("1.2.0"))
    val unknownVersion      = latestVersionButOld.copy(currentVersion = None)
    val determineSeverityOf = determineSeverity(recentDate)

    List(
      determineSeverityOf(latestVersionButOld),
      determineSeverityOf(olderVersion),
      determineSeverityOf(unknownVersion)
    ) should contain noneOf (Severity.Unknown, Severity.None, Severity.Low)
  }

object severitySpec:
  val oldDate    = DateTime.parse("2022-04-02")
  val recentDate = DateTime.parse("2023-05-03")
  val recentDependency = DependencyReport(
    name = "recent-dependency",
    currentVersion = Some("1.2.3"),
    latestVersion = "1.2.3",
    latestReleaseDate = Some(DateTime.parse("2023-05-02")),
    vulnerabilities = List.empty
  )
