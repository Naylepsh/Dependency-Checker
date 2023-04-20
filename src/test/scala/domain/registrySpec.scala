package domain

import scala.util.Try

import infra.json
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.*

import flatspec.*
import matchers.*
import registry.*

class RegistrySpec extends AnyFlatSpec with should.Matchers:
  import DependencySource.*

  "Parsing registry's source config" should "handle txt config" in {
    val config = """{
    |   "$type": "domain.registry.DependencySource.TxtSource",
    |   "path": "/dev/foo/requirements.txt"
    |}
    """.stripMargin

    json.parse[DependencySource](config) shouldBe TxtSource(
      "/dev/foo/requirements.txt"
    )
  }

  it should "handle toml config without group" in {
    val config = """{
    |   "$type": "domain.registry.DependencySource.TomlSource",
    |   "path": "/dev/foo/pyproject.toml"
    |}
    """.stripMargin

    json.parse[DependencySource](config) shouldBe TomlSource(
      "/dev/foo/pyproject.toml",
      None
    )
  }

  it should "handle toml config with group" in {
    val config = """{
    |   "$type": "domain.registry.DependencySource.TomlSource",
    |   "path": "/dev/foo/pyproject.toml",
    |   "group": ["group.dev"]
    |}
    """.stripMargin

    json.parse[DependencySource](config) shouldBe TomlSource(
      "/dev/foo/pyproject.toml",
      Some("group.dev")
    )
  }

  it should "throw when parsing unregistered format" in {
    val config = """{
    |   "$type": "this.path.points.to.nothing",
    |   "path": "/dev/foo/pyproject.toml",
    |   "group": ["group.dev"]
    |}
    """.stripMargin

    Try(json.parse[DependencySource](config)).isFailure shouldBe true
  }

  it should "throw when input is missing fields" in {
    val config = """{
    |   "$type": "domain.registry.DependencySource.TomlSource",
    |}
    """.stripMargin

    Try(json.parse[DependencySource](config)).isFailure shouldBe true
  }
