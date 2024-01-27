package parsers.python

import cats.implicits.*
import org.scalatest.*

import flatspec.*
import matchers.*

class RequirementsSpec extends AnyFlatSpec with should.Matchers:
  import RequirementsSpec.*

  "Replacing existing dependency" should "replace version" in:
    val initialTxtContent  = requirementsTxtTemplate("foo", "==", "1.2.3")
    val expectedTxtContent = requirementsTxtTemplate("foo", "==", "2.0.0")

    val actualTxtContent1 = requirements.update(
      "foo",
      "1.2.3",
      "2.0.0",
      initialTxtContent
    )
    val actualTxtContent2 = requirements.update(
      "foo",
      "^1.2.3",
      "2.0.0",
      initialTxtContent
    )
    val actualTxtContent3 = requirements.update(
      "foo",
      "~1.2.3",
      "2.0.0",
      initialTxtContent
    )
    actualTxtContent1 shouldBe Right(expectedTxtContent)
    actualTxtContent2 shouldBe Right(expectedTxtContent)
    actualTxtContent3 shouldBe Right(expectedTxtContent)

  "Replacing existing dependency with extras" should "replace version" in:
    val initialTxtContent  = requirementsTxtTemplate("foo[bar]", "==", "1.2.3")
    val expectedTxtContent = requirementsTxtTemplate("foo[bar]", "==", "2.0.0")

    val actualTxtContent = requirements.update(
      "foo",
      "1.2.3",
      "2.0.0",
      initialTxtContent
    )

    actualTxtContent shouldBe Right(expectedTxtContent)

  "Replacing non-existing dependency" should "fail" in:
    val initialTxtContent = requirementsTxtTemplate("foo", "==", "1.2.3")

    val actualTxtContent = requirements.update(
      "bar",
      "1.2.3",
      "2.0.0",
      initialTxtContent
    )

    actualTxtContent.isLeft shouldBe true

  "Replacing dependency with a different current version" should "fail" in:
    val initialTxtContent = requirementsTxtTemplate("foo", "==", "1.2.3")

    val actualTxtContent = requirements.update(
      "foo",
      "1.4.5",
      "2.0.0",
      initialTxtContent
    )

    actualTxtContent.isLeft shouldBe true

  "Replacing dependency with similar-but-not-exact-matching name" should "fail" in:
    val initialTxtContent = requirementsTxtTemplate("foo", "==", "1.2.3")

    val actualTxtContent1 = requirements.update(
      "foobar",
      "1.2.3",
      "2.0.0",
      initialTxtContent
    )
    val actualTxtContent2 = requirements.update(
      "barfoo",
      "1.2.3",
      "2.0.0",
      initialTxtContent
    )

    actualTxtContent1.isLeft shouldBe true
    actualTxtContent2.isLeft shouldBe true

object RequirementsSpec:
  def requirementsTxtTemplate(
      name: String,
      symbol: "==" | ">=" | "",
      version: String
  ): PackageManagementFiles.RequirementFile =
    val content = s"""
      |a==1.2.3
      |$name$symbol$version
      |b>=1.2.3
      |""".stripMargin
    PackageManagementFiles.RequirementFile(content)

  val requirements = Requirements.make
