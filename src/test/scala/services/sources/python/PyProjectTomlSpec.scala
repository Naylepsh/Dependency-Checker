package services.sources.python

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import flatspec._
import matchers._

class PyProjectTomlSpec extends AnyFlatSpec with should.Matchers {
  import PyProjectToml._

  "Parse" should "extract dependencies only from all sections containing 'dependencies' keyword" in {
    val fileContents = """
      |[tool.poetry]
      |name = "foo"
      |version = "0.1.0"
      |description = ""
      |authors = ["me <author@example.com>"]
      |
      |[tool.poetry.dependencies]
      |foo = "^2.6.0"
      |bar = "^3.8"
      |
      |[tool.poetry.dev-dependencies]
      |baz = "~3.4.5"
      |
      |[build-system]
      |requires = ["poetry-core>=1.0.0"]
      |build-backend = "poetry.core.masonry.api"
    """.stripMargin

    val parsed = extract(fileContents).get
    val names = parsed.map(_.name)
    val versions = parsed.map(_.currentVersion)

    names should contain only ("foo", "bar", "baz")
    versions should contain only (Some("^3.8"), Some("^2.6.0"), Some("~3.4.5"))
  }

  it should "ignore python" in {
    val fileContents = """
    |[tool.poetry.dependencies]
    |python = "^3.8"
    |foo = "^2.6.0"
    """.stripMargin

    val names = extract(fileContents).get.map(_.name)

    names should not contain "python"
  }
}
