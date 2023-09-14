package advisory

import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import core.domain.project.VulnerabilitySeverity

class GithubAdvisorySpec extends AsyncFreeSpec with AsyncIOSpec
    with Matchers:

  import GithubAdvisorySpec.*

  "Extract severity of critical vulnerability" in:
    advisory
      .getVulnerabilitySeverity(mongoosePrototypePollution)
      .asserting: severity =>
        severity shouldBe Some(VulnerabilitySeverity.Critical)

object GithubAdvisorySpec:
  val mongoosePrototypePollution = GHSAId("GHSA-9m93-w8w6-76hh").get
  val advisory                   = GithubAdvisory.make[IO]
