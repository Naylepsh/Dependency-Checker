package Dependencies.Python

import upickle.default.{ReadWriter => RW, macroRW}
import Dependencies.Dependency
import Dependencies.Utils
import scala.util.Try

object Pypi {

  case class PackageInfo(version: String) {}
  object PackageInfo {
    implicit val rw: RW[PackageInfo] = macroRW
  }

  case class PackageRelease(upload_time: String, requires_python: String) {
    val uploadTime = upload_time
    val requiresPython = requires_python
  }
  object PackageRelease {
    implicit val rw: RW[PackageRelease] = macroRW
  }

  case class PackageVulnerability(id: String, details: String)
  object PackageVulnerability {
    implicit val rw: RW[PackageVulnerability] = macroRW
  }

  case class PypiResponse(
      info: PackageInfo,
      releases: Map[String, List[PackageRelease]]
  )
  object PypiResponse {
    implicit val rw: RW[PypiResponse] = macroRW
  }

  case class VulnerabilitiesResponse(
      vulnerabilities: List[PackageVulnerability]
  )
  object VulnerabilitiesResponse {
    given rw: RW[VulnerabilitiesResponse] = macroRW
  }

  private def getLatestVersion(response: PypiResponse): Option[String] =
    response.releases.toList
      .collect {
        case items if items._2.nonEmpty =>
          (items._1, items._2.head.uploadTime)
      }
      .sortBy(_._2)(Ordering.String.reverse)
      .map(_._1)
      .headOption

  private def getVulnerabilities(
      dependency: Dependency
  ): Try[List[PackageVulnerability]] = Try {
    val resource = dependency.currentVersion match {
      case Some(version) => s"${dependency.name}/$version"
      case None          => dependency.name
    }
    val response =
      requests.get(s"https://pypi.org/pypi/$resource/json")
    val parsedResponse =
      Utils.JSON.parse[VulnerabilitiesResponse](response.text())

    parsedResponse.vulnerabilities
  }

  def getDependencyDetails(dependency: Dependency): Try[PackageDetails] =
    Try {
      val response =
        requests.get(s"https://pypi.org/pypi/${dependency.name}/json")
      val parsedResponse = Utils.JSON.parse[PypiResponse](response.text())

      val latestVersion = getLatestVersion(parsedResponse)
      val requiredPython = for {
        version <- latestVersion
        release <- parsedResponse.releases.get(version).flatMap(_.headOption)
        value <- Option(release.requiresPython)
      } yield value
      val vulnerabilities =
        getVulnerabilities(dependency).getOrElse(List()).map(_.id)

      PackageDetails(
        latestVersion,
        vulnerabilities,
        requiredPython
      )
    }
}
