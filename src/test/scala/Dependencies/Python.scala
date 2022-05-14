package Dependencies

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

  "Pypi response" should "be transformable from json-string to appropriate case class" in {
    import Pypi._
    import Utils.JSON

    val response = """{
      "info": {"version": "1.2.3"}, 
      "releases": {"0.0.1": [{"url": "localhost:8080/foo/bar"}]},
      "vulnerabilities": [
        {"id": "PYSEC-2021-9", "details": "Some desc. here"},
        {"id": "PYSEC-2021-8", "details": "More data here"}
      ] 
    }
    """

    val parsed = JSON.parse[PypiResponse](response)

    parsed.info.version shouldBe "1.2.3"
  }
}
