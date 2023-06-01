package core.infra.parsers.python

import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.*

import flatspec.*
import matchers.*

class PyProjectTomlSpec extends AnyFlatSpec with should.Matchers:
  import PyProjectToml.*

  "Extract without a group name" should """
    extract dependencies only from all sections containing 'dependencies' keyword""" in {
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

    val parsed   = extract(None)(fileContents).get
    val names    = parsed.map(_.name)
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

    val names = extract(None)(fileContents).get.map(_.name)

    names should not contain "python"
  }

  it should "extract only name and version" in {
    val fileContents = """
    |[tool.poetry.dev-dependencies]
    |black = {version = "^22.6.0", allow-prereleases = true}
    """.stripMargin

    val versions = extract(None)(fileContents).get.map(_.currentVersion)

    versions should contain only (Some("^22.6.0"))
  }

  "Extract with a group name" should """
    extract dependencies of specific group only""" in {
    val fileContents = """
    |[tool.poetry.dependencies]
    |python = "^3.8"
    |foo = "^2.6.0"
    |
    |[tool.poetry.dev-dependencies]
    |black = {version = "^22.6.0", allow-prereleases = true}
    """.stripMargin

    val names =
      extract(Some("tool.poetry.dependencies"))(fileContents).get.map(_.name)

    names should contain only "foo"
  }
