package persistence

import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Tag
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import core.infra.resources.database.*
import core.domain.project.ProjectScanConfig
import core.domain.project.Project
import core.domain.dependency.DependencySource
import core.application.config.AppConfig
import cats.effect.kernel.Resource
import core.domain.project.ScanResult
import core.domain.dependency.DependencyReport
import core.domain.dependency.DependencyDetails
import core.domain.dependency.Dependency
import core.domain.dependency.DependencyLatestRelease
import core.domain.Grouped
import persistence.DependencyRepository
import persistenece.ScanResultRepository
import ScanResultRepository.ScanResultRepositorySQL.GetAllResult
import org.legogroup.woof.{ *, given }
import core.domain.Time
import org.scalatest.Checkpoints.Checkpoint
import org.scalatest.Succeeded
import org.joda.time.DateTime
import core.domain.project.ScanReport

class ScanResultRepositorySpec extends AsyncFreeSpec with AsyncIOSpec
    with Matchers:
  import ScanResultRepositorySpec.*

  "Save, purge old and find latest scan report" taggedAs (DatabaseTest) in:
    transactor.use: xa =>
      val countScans =
        sql"SELECT COUNT(DISTINCT timestamp) FROM project_dependency"
          .query[Int]
          .unique
          .transact(xa)
      given Filter  = Filter.everything
      given Printer = NoColorPrinter()
      for
        given Logger[IO] <- DefaultLogger.makeIo(noop)
        dependencyRepository = DependencyRepository.make(xa)
        repository           = ScanResultRepository.make(xa, dependencyRepository)
        dt1                  <- Time[IO].currentDateTime
        _                    <- repository.save(List(firstResult), dt1)
        dt2                  <- Time[IO].currentDateTime
        _                    <- repository.save(List(secondResult), dt2)
        dt3                  <- Time[IO].currentDateTime
        _                    <- repository.save(List(thirdResult), dt3)
        savedCount           <- countScans
        _                    <- repository.deleteOld(project.name)
        savedCountAfterPurge <- countScans
        latestScan           <- repository.getLatestScanReport(project.name)
      yield
        val cp = Checkpoint()
        savedCount shouldBe 3
        savedCountAfterPurge shouldBe 1
        println(s"Latest scan: $latestScan")
        latestScan.map(_.dependenciesReports.head.items.head) shouldBe Some(
          thirdResult.dependenciesReports.head.items.head
        )
        cp.reportAll()
        Succeeded

  "GetAllResult.toDomain constructs a proper report" in:
    val scanReports = GetAllResult.toDomain(testGetAllResults, latestReleases)
    scanReports should contain only (expectedFirstProjectReport, expectedSecondProjectReport)

object ScanResultRepositorySpec:
  val transactor = Resource.eval(AppConfig.load[IO]).flatMap: config =>
    makeSqliteTransactorResource[IO](config.database).evalTap: xa =>
      val freshStart =
        for
          _ <- sql"DELETE FROM project".update.run
          _ <- sql"DELETE FROM dependency".update.run
        yield ()
      freshStart.transact(xa)

  val now = DateTime.now()
  val project    = Project("420", "foo")
  val dependency = Dependency("bar", Some("1.0.0"))

  private def makeResult(latestVersion: String) =
    ScanResult(
      project,
      List(Grouped(
        "requirements.txt",
        List(
          DependencyReport(
            dependency,
            DependencyDetails(
              dependency.name,
              dependency.currentVersion.getOrElse("-"),
              latestVersion,
              Some(now)
            ),
            None
          )
        )
      ))
    )

  val firstResult  = makeResult("4.2.0")
  val secondResult = makeResult("4.2.1")
  val thirdResult  = makeResult("4.2.2")

  val noop: Output[IO] = new:
    override def output(str: String): IO[Unit]      = IO.unit
    override def outputError(str: String): IO[Unit] = IO.unit

  val expectedFirstProjectReport = ScanReport(
    projectName = "first-project",
    dependenciesReports = List(
      Grouped(
        groupName = "requirements.txt",
        items = List(DependencyReport(
          name = "Django",
          currentVersion = Some("1.2.3"),
          latestVersion = "4.5.6",
          currentVersionReleaseDate = Some(now),
          latestReleaseDate = Some(now),
          vulnerabilities =
            List("first-vulnerability", "second-vulnerability"),
          notes = Some("requires python>=4.20")
        ))
      )
    )
  )
  val expectedSecondProjectReport = ScanReport(
    projectName = "second-project",
    dependenciesReports = List(
      Grouped(
        groupName = "requirements.txt",
        items = List(DependencyReport(
          name = "Flask",
          currentVersion = Some("2.3.4"),
          latestVersion = "2.3.5",
          currentVersionReleaseDate = Some(now),
          latestReleaseDate = Some(now),
          vulnerabilities = List.empty,
          notes = None
        ))
      )
    )
  )

  val latestReleases = List(
    DependencyLatestRelease("Django", "4.5.6", now),
    DependencyLatestRelease("Flask", "2.3.5", now)
  )

  val testGetAllResults = List(
    GetAllResult(
      projectName = "first-project",
      groupName = "requirements.txt",
      dependencyId = "1",
      dependencyName = "Django",
      dependencyVersion = Some("1.2.3"),
      dependencyReleaseDate = Some(now),
      dependencyNotes = Some("requires python>=4.20"),
      dependencyVulnerability = Some("first-vulnerability")
    ),
    GetAllResult(
      projectName = "first-project",
      groupName = "requirements.txt",
      dependencyId = "1",
      dependencyName = "Django",
      dependencyVersion = Some("1.2.3"),
      dependencyReleaseDate = Some(now),
      dependencyNotes = Some("requires python>=4.20"),
      dependencyVulnerability = Some("second-vulnerability")
    ),
    GetAllResult(
      projectName = "second-project",
      groupName = "requirements.txt",
      dependencyId = "1",
      dependencyName = "Flask",
      dependencyVersion = Some("2.3.4"),
      dependencyReleaseDate = Some(now),
      dependencyNotes = None,
      dependencyVulnerability = None
    )
  )
