package advisory

import cats.effect.Sync
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.dsl.DSL.Parse.*
import net.ruippeixotog.scalascraper.dsl.DSL.*
import core.domain.vulnerability.VulnerabilitySeverity

opaque type GHSAId = String
object GHSAId:
  def apply(name: String): Option[GHSAId] =
    if name.startsWith("GHSA-") then Some(name) else None

trait GithubAdvisory[F[_]]:
  def getVulnerabilitySeverity(vulnerabilityId: GHSAId)
      : F[Option[VulnerabilitySeverity]]

object GithubAdvisory:
  def make[F[_]: Sync]: GithubAdvisory[F] = new:
    private val browser = JsoupBrowser()

    def getVulnerabilitySeverity(vulnerabilityId: GHSAId)
        : F[Option[VulnerabilitySeverity]] =
      Sync[F]
        .delay(browser.get(s"https://github.com/advisories/$vulnerabilityId"))
        .map: doc =>
          for
            element     <- doc >?> element("""span[title^="Severity"]""")
            rawSeverity <- element.text.split(" ").headOption
            severity    <- convertToSeverity(rawSeverity)
          yield severity

    private def convertToSeverity(str: String): Option[VulnerabilitySeverity] =
      str match
        case "Low"      => VulnerabilitySeverity.Low.some
        case "Moderate" => VulnerabilitySeverity.Medium.some
        case "High"     => VulnerabilitySeverity.High.some
        case "Critical" => VulnerabilitySeverity.Critical.some
        case _          => None
