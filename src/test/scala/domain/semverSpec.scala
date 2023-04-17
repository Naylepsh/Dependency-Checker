package domain

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import flatspec._
import matchers._
import scala.util.{Success, Try}

class SemverSpec extends AnyFlatSpec with should.Matchers {
  import semver._

  "calculate version difference" should "detect major differences" in {
    calculateVersionDifference(
      "1.2.3",
      "2.2.3"
    ).value shouldBe VersionDifference.Major
  }

  it should "detect minor differences" in {
    calculateVersionDifference(
      "1.2.3",
      "1.3.3"
    ).value shouldBe VersionDifference.Minor
  }

  it should "detect patch differences" in {
    calculateVersionDifference(
      "1.2.3",
      "1.2.4"
    ).value shouldBe VersionDifference.Patch
  }

  it should "detect when there are no differences" in {
    calculateVersionDifference("1.2.3", "1.2.3") shouldBe None
  }

  it should "handle non-purely-numeric versions" in {
    calculateVersionDifference(
      "1.2b.3",
      "1.2.3"
    ).value shouldBe VersionDifference.Minor
  }

  it should "handle semantic versioning symbols" in {
    calculateVersionDifference("^1.2.3", "1.3.3") shouldBe None
    calculateVersionDifference("~1.2.3", "1.2.4") shouldBe None
  }
}
