package Dependencies

import org.scalatest._
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import flatspec._
import matchers._
import scala.util.Success
import scala.util.Try

class PackageSpec extends AnyFlatSpec with should.Matchers {
  "calculate version difference" should "detect major differences" in {
    calculate_version_difference(
      "1.2.3",
      "2.2.3"
    ).success.value shouldBe Some(VersionDifference.Major)
  }

  it should "detect minor differences" in {
    calculate_version_difference(
      "1.2.3",
      "1.3.3"
    ).success.value shouldBe Some(VersionDifference.Minor)
  }

  it should "detect patch differences" in {
    calculate_version_difference(
      "1.2.3",
      "1.2.4"
    ).success.value shouldBe Some(VersionDifference.Patch)
  }

  it should "detect when there are no differences" in {
    calculate_version_difference("1.2.3", "1.2.3").success.value shouldBe None
  }

  it should "have no result on non-purely-numeric versions" in {
    calculate_version_difference("1.2b.3", "1.2.3").success.value shouldBe None
  }

  it should "handle semantic versioning symbols" in {
    calculate_version_difference("^1.2.3", "1.3.3").success.value shouldBe None
    calculate_version_difference("~1.2.3", "1.2.4").success.value shouldBe None
  }
}
