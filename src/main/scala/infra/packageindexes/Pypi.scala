package infra.packageindexes

import domain.PackageIndex
import domain.dependency.Dependency
import domain.dependency.DependencyDetails
import io.circe.generic.semiauto.*
import io.circe.*
import sttp.client3.*
import sttp.client3.circe.*
import sttp.capabilities.WebSockets
import cats.implicits.*
import cats.Monad
import cats.effect.Sync
import scala.concurrent.duration.*

object Pypi:
  case class PackageInfo(version: String)
  object PackageInfo:
    given Decoder[PackageInfo] = deriveDecoder

  case class PackageRelease(
      upload_time: String,
      requires_python: Option[String]
  )
  object PackageRelease:
    given Decoder[PackageRelease] = deriveDecoder

  case class PackageVulnerability(id: String, details: String)
  object PackageVulnerability:
    given Decoder[PackageVulnerability] = deriveDecoder

  case class Package(
      info: PackageInfo,
      releases: Map[String, List[PackageRelease]],
      vulnerabilities: List[PackageVulnerability]
  )
  object Package:
    given Decoder[Package] = deriveDecoder

  case class VulnerabilitiesResponse(
      vulnerabilities: List[PackageVulnerability]
  )
  object VulnerabilitiesResponse:
    given Decoder[VulnerabilitiesResponse] = deriveDecoder

class Pypi[F[_]: Monad: Sync](backend: SttpBackend[F, WebSockets])
    extends PackageIndex[F]:
  import Pypi.*

  override def getDetails(dependency: Dependency)
      : F[Either[String, DependencyDetails]] =
    (
      getLatestDependencyInfo(dependency),
      getVulnerabilities(dependency)
    ).tupled.map((_, _).tupled.map {
      case (packageData, vulnerabilities) =>
        val latestVersion = packageData.info.version
        val requiredPython = packageData.releases
          .get(latestVersion)
          .flatMap(_.headOption.flatMap(_.requires_python))

        DependencyDetails(
          dependency.name,
          dependency.currentVersion.getOrElse(latestVersion),
          latestVersion,
          vulnerabilities.map(_.id),
          requiredPython
        )
    })

  private def getLatestDependencyInfo(
      dependency: Dependency
  ): F[Either[String, Package]] =
    val infoEndpoint = uri"https://pypi.org/pypi/${dependency.name}/json"
    basicRequest
      .get(infoEndpoint)
      .readTimeout(10.seconds)
      .response(asJson[Package])
      .send(backend)
      .map(_.body.leftMap(buildErrorMessage(infoEndpoint)))

  private def getVulnerabilities(
      dependency: Dependency
  ): F[Either[String, List[PackageVulnerability]]] =
    val coreEndpoint = "https://pypi.org/pypi"
    val endpoint = dependency.currentVersion match
      case Some(version) =>
        s"$coreEndpoint/${dependency.name}/${cleanupVersion(version)}"
      case None => s"$coreEndpoint/${dependency.name}"
    val vulnerabilitiesEndpoint = uri"$endpoint/json"

    basicRequest
      .get(vulnerabilitiesEndpoint)
      .readTimeout(10.seconds)
      .response(asJson[VulnerabilitiesResponse])
      .send(backend)
      .map(_.body.leftMap(
        buildErrorMessage(vulnerabilitiesEndpoint)
      ).map(_.vulnerabilities))

  private def cleanupVersion(version: String): String =
    version
      // This is a temporary hack, for ~/^ version shoud be bumped to the latest appropriate one
      .replaceAll("[\\^~]", "")
      // Another hack, * should take the latest available version, not 0
      .replaceAll("\\*", "0")

  private def buildErrorMessage(url: sttp.model.Uri)(
      exception: ResponseException[
        String,
        io.circe.Error
      ]
  ): String =
    s"url: ${url.toString}, ${exception.getMessage()}"
