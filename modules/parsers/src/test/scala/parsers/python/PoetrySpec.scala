package parsers.python

import cats.implicits.*
import org.scalatest.*

import flatspec.*
import matchers.*

class PoetrySpec extends AnyFlatSpec with should.Matchers:
  import Poetry.*
  import PoetrySpec.*

  "Replacing existing dependency" should "replace version" in:
    val initialTomlContent  = pyProjectTomlTemplate("foo", "^", "1.2.3")
    val expectedTomlContent = pyProjectTomlTemplate("foo", "^", "2.0.0")

    val actualTomlContent = updateDependency(
      "foo",
      "1.2.3",
      "2.0.0",
      initialTomlContent
    )

    actualTomlContent shouldBe expectedTomlContent

  "Replacing non-existing dependency" should "keep the file content the same" in:
    val initialTomlContent = pyProjectTomlTemplate("foo", "^", "1.2.3")

    val actualTomlContent = updateDependency(
      "bar",
      "1.2.3",
      "2.0.0",
      initialTomlContent
    )

    actualTomlContent shouldBe initialTomlContent

  "Replacing dependency with a different current version" should "keep the file content the same" in:
    val initialTomlContent = pyProjectTomlTemplate("foo", "^", "1.2.3")

    val actualTomlContent = updateDependency(
      "foo",
      "1.4.5",
      "2.0.0",
      initialTomlContent
    )

    actualTomlContent shouldBe initialTomlContent

  "Replacing dependency with similar-but-not-exact-matching name" should "keep the file content the same" in:
    val initialTomlContent = pyProjectTomlTemplate("foo", "^", "1.2.3")

    val actualTomlContent1 = updateDependency(
      "foobar",
      "1.2.3",
      "2.0.0",
      initialTomlContent
    )
    val actualTomlContent2 = updateDependency(
      "barfoo",
      "1.2.3",
      "2.0.0",
      initialTomlContent
    )

    actualTomlContent1 shouldBe initialTomlContent
    actualTomlContent2 shouldBe initialTomlContent

object PoetrySpec:
  def pyProjectTomlTemplate(
      name: String,
      symbol: "^" | "",
      version: String
  ): String = s"""
  |a = "1.2.3"
  |$name = "$symbol$version"
  |b = "1.2.3"
  |""".stripMargin
