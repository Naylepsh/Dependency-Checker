package core.domain

import cats.implicits.*
import org.scalatest.*

import flatspec.*
import matchers.*
import registry.*
import dependency.DependencyReport

class DeltaSpec extends AnyFlatSpec with should.Matchers:
  import delta.*
  import project.*
  import DeltaSpec.*

  "Creating a delta of different projects" should "fail" in {
    val a = ScanReport("some-project", List.empty)
    val b = ScanReport("other-project", List.empty)

    ScanDelta(a, b).isInvalid shouldBe true
  }

  "Creating a delta of project with no groups on the left" should "create an empty delta" in {
    val scan = ScanReport("some-project", List.empty)

    List(
      scan,
      scan.copy(dependenciesReports = List(emptyGroup)),
      scan.copy(dependenciesReports = List(nonEmptyGroup))
    ).foreach(otherScan =>
      val delta = ScanDelta(scan, otherScan)

      delta.map(_.projectName) shouldBe scan.projectName.validNel[String]
      delta.map(_.dependenciesDeltas.length) shouldBe 0.validNel[String]
    )
  }

  "Creating a delta of project with matching groups" should "create a non-empty delta" in {
    val scan      = ScanReport("some-project", List(nonEmptyGroup))
    val laterScan = scan.copy(dependenciesReports = List(laterNonEmptyGroup))

    val delta = ScanDelta(scan, laterScan)

    delta.map(_.projectName) shouldBe scan.projectName.validNel[String]
    delta.map(_.dependenciesDeltas.length) shouldBe 1.validNel[String]
  }

object DeltaSpec:
  val emptyGroup = Grouped("some-group", List.empty[DependencyReport])
  val report = DependencyReport(
    "foo",
    Some("0.1.2"),
    "1.2.3",
    None,
    None,
    List.empty,
    None
  )
  val nonEmptyGroup = emptyGroup.copy(items = List(report))
  val laterNonEmptyGroup = nonEmptyGroup.copy(items =
    List(report.copy(currentVersion = Some("1.2.3")))
  )
