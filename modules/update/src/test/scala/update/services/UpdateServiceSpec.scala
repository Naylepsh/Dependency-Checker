package update.services

import cats.syntax.all.*
import core.domain.dependency.DependencyVulnerability
import core.domain.severity
import core.domain.update.DependencyToUpdate
import org.scalatest.*

import flatspec.*
import matchers.*

class UpdateServiceSpec extends AnyFlatSpec with should.Matchers:
  import UpdateServiceSpec.*

  "Dependency with vulnerabilities and not up-to-date version" should "be marked for update" in:
    List(
      outdatedDependencyWithVulnerabilities,
      outdatedDependencyWithVulnerabilitiesAndSemverMark
    ).foreach: dependency =>
      UpdateService.shouldUpdate(dependency) shouldBe true

  "(Semi) up-to-date dependency without vulnerabilities" should "not be marked for update" in:
    List(
      dependencyWithinSemverRangeWithoutVulnerabilities,
      upToDateDependencyWithoutVulnerabilities
    ).foreach: dependency =>
      UpdateService.shouldUpdate(dependency) shouldBe false

  "Up-to-date dependency with vulnerabilities" should "not be marked for update (because there's nothing to update to)" in:
    UpdateService.shouldUpdate(
      upToDateDependencyWithVulnerabilities
    ) shouldBe false

object UpdateServiceSpec:
  val vulnerability = DependencyVulnerability(
    name = "CVE-123",
    severity = core.domain.vulnerability.VulnerabilitySeverity.Medium.some
  )
  val outdatedDependencyWithVulnerabilities = DependencyToUpdate(
    name = "foo",
    currentVersion = "1.2.3".some,
    latestVersion = "1.2.4",
    vulnerabilities = List(vulnerability)
  )
  val outdatedDependencyWithVulnerabilitiesAndSemverMark = DependencyToUpdate(
    name = "foo",
    currentVersion = "^1.2.3".some,
    latestVersion = "1.2.4",
    vulnerabilities = List(vulnerability)
  )
  val dependencyWithinSemverRangeWithoutVulnerabilities = DependencyToUpdate(
    name = "foo",
    currentVersion = "^1.2.3".some,
    latestVersion = "1.2.4",
    vulnerabilities = List.empty
  )
  val upToDateDependencyWithoutVulnerabilities = DependencyToUpdate(
    name = "foo",
    currentVersion = "1.2.4".some,
    latestVersion = "1.2.4",
    vulnerabilities = List.empty
  )
  val upToDateDependencyWithVulnerabilities = DependencyToUpdate(
    name = "foo",
    currentVersion = "1.2.4".some,
    latestVersion = "1.2.4",
    vulnerabilities = List(vulnerability)
  )
