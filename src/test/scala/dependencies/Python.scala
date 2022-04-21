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
  
  it should "extract a name that uses special characters" in {
    val requirements = """
    | django-autocomplete-light
    """.stripMargin

    val dependencies = parseRequirements(requirements)
    dependencies should contain(
      Dependency(
        name = "django-autocomplete-light",
        currentVersion = None,
        latestVersion = None
      )
    )
    dependencies should have length 1
  }

  "Pip: Parse dependency versions" should "extract all versions" in {
    import Pip._

    // This is a partial output of `pip install Django==`
    val versionsText =
      "requirement Django== (from versions: 1.1.3, 1.8b2, 2.1rc1) ERROR"

    val versions = parseDependencyVersions(versionsText)

    versions should have length 3
    versions should contain("1.1.3")
    versions should contain("1.8b2")
    versions should contain("2.1rc1")
  }

  "Pip: Get dependency versions" should "extract multiple versions of Django" in {
    import Pip._

    val versions = getDependencyVersions("Django")

    versions.length should be > 0
  }

  it should "return an empty list if a package name is invalid" in {
    import Pip._

    val versions = getDependencyVersions("Djangooooooooo")

    versions should have length 0
  }

  "Get latest version" should "pick the latest from available versions" in {
    val versions = List("1.1.1", "1.2.2", "0.0.1")

    val latestVersion = getLatestVersion(versions)

    latestVersion.value shouldBe "1.2.2"
  }

  // it should "not allow latest version to be earlier than current version" in {

  // }
}
