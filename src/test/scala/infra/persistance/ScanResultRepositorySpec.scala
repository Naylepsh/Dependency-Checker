package infra.persistance

import core.domain.Grouped
import core.domain.dependency.DependencyReport
import core.domain.project.ScanReport
import infra.persistance.ScanResultRepository.ScanResultRepositorySQL.GetAllResult
import org.joda.time.DateTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*

class ScanResultRepositorySpec extends AnyFlatSpec with should.Matchers:
  import ScanResultRepositorySpec.*

  "GetAllResult.toDomain" should "construct a proper report" in {
    val scanReports = GetAllResult.toDomain(testGetAllResults)
    scanReports should contain only (expectedFirstProjectReport, expectedSecondProjectReprort)
  }

object ScanResultRepositorySpec:
  val now = DateTime.now()
  val expectedFirstProjectReport = ScanReport(
    projectName = "first-project",
    dependenciesReports = List(
      Grouped(
        groupName = "requirements.txt",
        items = List(DependencyReport(
          name = "Django",
          currentVersion = Some("1.2.3"),
          latestVersion = "4.5.6",
          latestReleaseDate = Some(now),
          vulnerabilities =
            List("first-vulnerability", "second-vulnerability"),
          notes = Some("requires python>=4.20")
        ))
      )
    )
  )
  val expectedSecondProjectReprort = ScanReport(
    projectName = "second-project",
    dependenciesReports = List(
      Grouped(
        groupName = "requirements.txt",
        items = List(DependencyReport(
          name = "Flask",
          currentVersion = Some("2.3.4"),
          latestVersion = "2.3.5",
          latestReleaseDate = Some(now),
          vulnerabilities = List.empty,
          notes = None
        ))
      )
    )
  )

  val testGetAllResults = List(
    GetAllResult(
      projectName = "first-project",
      groupName = "requirements.txt",
      dependencyId = "1",
      dependencyName = "Django",
      dependencyCurrentVersion = Some("1.2.3"),
      dependencyLatestVersion = "4.5.6",
      dependencyLatestReleaseDate = Some(now),
      dependencyNotes = Some("requires python>=4.20"),
      dependencyVulnerability = Some("first-vulnerability")
    ),
    GetAllResult(
      projectName = "first-project",
      groupName = "requirements.txt",
      dependencyId = "1",
      dependencyName = "Django",
      dependencyCurrentVersion = Some("1.2.3"),
      dependencyLatestVersion = "4.5.6",
      dependencyLatestReleaseDate = Some(now),
      dependencyNotes = Some("requires python>=4.20"),
      dependencyVulnerability = Some("second-vulnerability")
    ),
    GetAllResult(
      projectName = "second-project",
      groupName = "requirements.txt",
      dependencyId = "1",
      dependencyName = "Flask",
      dependencyCurrentVersion = Some("2.3.4"),
      dependencyLatestVersion = "2.3.5",
      dependencyLatestReleaseDate = Some(now),
      dependencyNotes = None,
      dependencyVulnerability = None
    )
  )
