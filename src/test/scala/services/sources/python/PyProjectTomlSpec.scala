package services.sources.python

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import flatspec._
import matchers._

class PyProjectTomlSpec extends AnyFlatSpec with should.Matchers {
  import PyProjectToml._

  "Parse" should "extract dependencies only from [tools.poetry.dependencies] section" in {
    val fileContents = """
      |[tool.poetry]
      |name = "foo"
      |version = "0.1.0"
      |description = ""
      |authors = ["me <author@example.com>"]
      |
      |[tool.poetry.dependencies]
      |python = "^3.8"
      |python-foo = "^2.6.0"
      |
      |[tool.poetry.dev-dependencies]
      |bar = "~3.4.5"
      |
      |[build-system]
      |requires = ["poetry-core>=1.0.0"]
      |build-backend = "poetry.core.masonry.api"
    """.stripMargin

    val parsed = extract(fileContents).get

    parsed.length shouldBe 2
    parsed.map(_.name) should contain("python")
    parsed.map(_.currentVersion) should contain(Some("^3.8"))
    parsed.map(_.name) should contain("python-foo")
  }
}
