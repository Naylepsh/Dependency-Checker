package scanning.infra.packageindexes

import scala.concurrent.duration.*

import cats.Monad
import cats.effect.Sync
import cats.implicits.*
import com.github.nscala_time.time.Imports.*
import core.domain.dependency.{ Dependency, DependencyDetails }
import io.circe.*
import io.circe.generic.semiauto.*
import scanning.domain.PackageIndex
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.client3.circe.*
import cats.data.NonEmptyList
import org.joda.time.DateTime

class Pypi[F[_]: Monad: Sync](backend: SttpBackend[F, WebSockets])
    extends PackageIndex[F]:
  import Pypi.*

  private val dateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")

  override def getDetails(dependency: Dependency)
      : F[Either[String, DependencyDetails]] =
    (
      getLatestDependencyInfo(dependency),
      getVulnerabilities(dependency)
    ).tupled.map((_, _).tupled.map: (packageData, vulnerabilities) =>
      val latestVersion = packageData.info.version
      val latestInfo = packageData.releases
        .get(latestVersion)
        .flatMap(_.headOption)

      DependencyDetails(
        dependency.name,
        dependency.currentVersion.getOrElse(latestVersion),
        packageData.urls.head.upload_time_iso_8601,
        latestVersion,
        latestInfo.map(_.upload_time_iso_8601),
        vulnerabilities.map(_.id),
        latestInfo.flatMap(_.requires_python)
      )
    )

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
      .map: response =>
        response
          .body
          .leftMap(buildErrorMessage(vulnerabilitiesEndpoint))
          .map(_.vulnerabilities)

  private def cleanupVersion(version: String): String =
    /**
     * We don't really know when the repo was last deployed,
     * so it's safer to assume the lowest possible version
     */
    version
      .replaceAll("[\\^~]", "")
      .replaceAll("\\*", "0")

  private def buildErrorMessage(url: sttp.model.Uri)(
      exception: ResponseException[
        String,
        io.circe.Error
      ]
  ): String =
    s"url: ${url.toString}, ${exception.getMessage()}"

object Pypi:
  given Decoder[DateTime] = Decoder.decodeString.emap: str =>
    Either
      .catchNonFatal:
        DateTime.parse(str)
      .leftMap: error =>
        error.toString

  case class PackageInfo(version: String) derives Decoder

  case class PackageRelease(
      upload_time_iso_8601: DateTime,
      requires_python: Option[String]
  ) derives Decoder

  case class PackageUrl(upload_time_iso_8601: DateTime) derives Decoder

  case class PackageVulnerability(id: String, details: String) derives Decoder

  case class Package(
      info: PackageInfo,
      releases: Map[String, List[PackageRelease]],
      urls: NonEmptyList[PackageUrl],
      vulnerabilities: List[PackageVulnerability]
  ) derives Decoder

  case class VulnerabilitiesResponse(
      vulnerabilities: List[PackageVulnerability]
  ) derives Decoder
