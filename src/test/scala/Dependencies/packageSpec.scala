package Dependencies

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import flatspec._
import matchers._
import scala.util.{Success, Try}

class PackageSpec extends AnyFlatSpec with should.Matchers {
  "calculate version difference" should "detect major differences" in {
    calculateVersionDifference(
      "1.2.3",
      "2.2.3"
    ).success.value shouldBe Some(VersionDifference.Major)
  }

  it should "detect minor differences" in {
    calculateVersionDifference(
      "1.2.3",
      "1.3.3"
    ).success.value shouldBe Some(VersionDifference.Minor)
  }

  it should "detect patch differences" in {
    calculateVersionDifference(
      "1.2.3",
      "1.2.4"
    ).success.value shouldBe Some(VersionDifference.Patch)
  }

  it should "detect when there are no differences" in {
    calculateVersionDifference("1.2.3", "1.2.3").success.value shouldBe None
  }

  it should "handle non-purely-numeric versions" in {
    calculateVersionDifference("1.2b.3", "1.2.3").success.value shouldBe Some(
      VersionDifference.Minor
    )
  }

  it should "handle semantic versioning symbols" in {
    calculateVersionDifference("^1.2.3", "1.3.3").success.value shouldBe None
    calculateVersionDifference("~1.2.3", "1.2.4").success.value shouldBe None
  }
}
