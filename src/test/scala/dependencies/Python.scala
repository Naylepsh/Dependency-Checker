package dependencies

import collection.mutable.Stack
import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import flatspec._
import matchers._

class PythonSpec extends AnyFlatSpec with should.Matchers {
  import Python._

  "Parse requirements" should "ignore commented out depenencies" in {
    val requirements = """
    | Django==1.2.3
    | # Flask==4.5.6
    """.stripMargin

    val dependencies = parseRequirements(requirements)
    dependencies should contain(
      Dependency(
        name = "Django",
        currentVersion = Some("1.2.3"),
        latestVersion = None
      )
    )
    dependencies should have length 1
  }

  it should "set current version to None when it's missing" in {
    val requirements = """
    | Django
    """.stripMargin

    val dependencies = parseRequirements(requirements)
    dependencies should contain(
      Dependency(
        name = "Django",
        currentVersion = None,
        latestVersion = None
      )
    )
    dependencies should have length 1
  }

  "Get latest version" should "pick the latest from available versions" in {
    val versions = List("1.1.1", "1.2.2", "0.0.1")

    val latestVersion = getLatestVersion(versions)

    latestVersion.value shouldBe "1.2.2"
  }

  // it should "not allow latest version to be earlier than current version" in {

  // }
}
