package domain

import scala.util.Try

import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.*
import io.circe.parser.decode

import flatspec.*
import matchers.*
import registry.*

class RegistrySpec extends AnyFlatSpec with should.Matchers:
  import DependencySource.*

  "Parsing registry's source config" should "handle txt config" in {
    val config = """{
    |   "type": "txt",
    |   "path": "/dev/foo/requirements.txt"
    |}
    """.stripMargin

    decode[DependencySource](config) shouldBe Right(TxtSource(
      "/dev/foo/requirements.txt"
    ))
  }

  it should "handle toml config without group" in {
    val config = """{
    |   "type": "toml",
    |   "path": "/dev/foo/pyproject.toml"
    |}
    """.stripMargin

    decode[DependencySource](config) shouldBe Right(TomlSource(
      "/dev/foo/pyproject.toml",
      None
    ))
  }

  it should "handle toml config with group" in {
    val config = """{
    |   "type": "toml",
    |   "path": "/dev/foo/pyproject.toml",
    |   "group": "group.dev"
    |}
    """.stripMargin

    decode[DependencySource](config) shouldBe Right(TomlSource(
      "/dev/foo/pyproject.toml",
      Some("group.dev")
    ))
  }

  it should "fail when parsing unregistered format" in {
    val config = """{
    |   "type": "this.path.points.to.nothing",
    |   "path": "/dev/foo/pyproject.toml",
    |   "group": "group.dev"
    |}
    """.stripMargin

    decode[DependencySource](config).isLeft shouldBe true
  }

  it should "fail when input is missing fields" in {
    val config = """{
    |   "type": "toml",
    |}
    """.stripMargin

    decode[DependencySource](config).isLeft shouldBe true
  }
