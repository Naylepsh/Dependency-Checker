package Dependencies.Python

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import flatspec._
import matchers._
import Dependencies.Dependency

class RequirementsTxtSpec extends AnyFlatSpec with should.Matchers {
  import RequirementsTxt._

  "Parse" should "ignore commented out depenencies" in {
    val requirements = """
    | Django==1.2.3
    | # Flask==4.5.6
    """.stripMargin

    val dependencies = parse(requirements)
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

    val dependencies = parse(requirements)
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

    val dependencies = parse(requirements)
    dependencies should contain(
      Dependency(
        name = "django-autocomplete-light",
        currentVersion = None,
        latestVersion = None
      )
    )
    dependencies should have length 1
  }
}
