package advisory

import core.domain.vulnerability.VulnerabilitySeverity
import cats.syntax.all.*
import cats.Applicative

trait Advisory[F[_]]:
  def getVulnerabilitySeverity(vulnerabilityName: String)
      : F[Option[VulnerabilitySeverity]]

object Advisory:
  def make[F[_]: Applicative](githubAdvisory: GithubAdvisory[F]): Advisory[F] =
    new:
      def getVulnerabilitySeverity(vulnerabilityName: String)
          : F[Option[VulnerabilitySeverity]] =
        GHSAId(vulnerabilityName) match
          case None     => None.pure
          case Some(id) => githubAdvisory.getVulnerabilitySeverity(id)
