package upkeep

import cats.implicits.*
import org.scalatest.*

import flatspec.*
import matchers.*

class DomainSpec extends AnyFlatSpec with should.Matchers:
  import domain.*
  import DomainSpec.*

  "Replacing existing dependency" should "replace version" in {
    val initialContent  = requirementsTxtTemplate("foo", "==", "1.2.3")
    val expectedContent = requirementsTxtTemplate("foo", "==", "2.0.0")
    replaceDependency(
      initialContent,
      "foo",
      "1.2.3",
      "2.0.0"
    ) shouldBe expectedContent
  }

  "Replacing non-existing dependency" should "keep the file content the same" in {
    val initialContent = requirementsTxtTemplate("foo", "==", "1.2.3")
    replaceDependency(
      initialContent,
      "bar",
      "1.2.3",
      "2.0.0"
    ) shouldBe initialContent
  }

  "Replacing dependency with a different current version" should "keep the file content the same" in {
    val initialContent = requirementsTxtTemplate("foo", "==", "1.2.3")
    replaceDependency(
      initialContent,
      "foo",
      "1.4.5",
      "2.0.0"
    ) shouldBe initialContent
  }

  "Replacing dependency with similar-but-not-exact-matching name" should "keep the file content the same" in {
    val initialContent = requirementsTxtTemplate("foo", "==", "1.2.3")
    replaceDependency(
      initialContent,
      "foobar",
      "1.2.3",
      "2.0.0"
    ) shouldBe initialContent
    replaceDependency(
      initialContent,
      "barfoo",
      "1.2.3",
      "2.0.0"
    ) shouldBe initialContent
  }

object DomainSpec:
  def requirementsTxtTemplate(
      name: String,
      symbol: "==" | ">=" | "",
      version: String
  ): String = s"""
  |a==1.2.3
  |$name$symbol$version
  |b>=1.2.3
  """.stripMargin
