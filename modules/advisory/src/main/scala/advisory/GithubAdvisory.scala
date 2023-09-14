package advisory

import cats.effect.Sync
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.dsl.DSL.Parse.*
import net.ruippeixotog.scalascraper.dsl.DSL.*

enum VulnerailitySeverity:
  case Low, Medium, High, Critical
object VulnerailitySeverity:
  def apply(str: String): Option[VulnerailitySeverity] =
    str match
      case "low"      => Low.some
      case "moderate" => Medium.some
      case "high"     => High.some
      case "critical" => Critical.some
      case _          => None

opaque type GHSAId = String
object GHSAId:
  def apply(name: String): Option[GHSAId] =
    if name.startsWith("GHSA-") then Some(name) else None

trait GithubAdvisory[F[_]]:
  def getVulnerabilitySeverity(vulnerabilityId: GHSAId)
      : F[Option[VulnerailitySeverity]]

object GithubAdvisory:
  def make[F[_]: Sync]: GithubAdvisory[F] = new:
    private val browser = JsoupBrowser()

    def getVulnerabilitySeverity(vulnerabilityId: GHSAId)
        : F[Option[VulnerailitySeverity]] =
      Sync[F]
        .delay(browser.get(s"https://github.com/advisories/$vulnerabilityId"))
        .map: doc =>
          for
            element     <- doc >?> element("""span[title^="Severity"]""")
            rawSeverity <- element.text.split(" ").headOption
            severity    <- VulnerailitySeverity.apply(rawSeverity)
          yield severity
