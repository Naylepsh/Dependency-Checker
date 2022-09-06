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
    getLatestDependencyInfo(dependency).flatMap(response => {
      val latestVersion = response.info.version
      val requiredPython = for {
        release <- response.releases
          .get(latestVersion)
          .flatMap(_.headOption)
        value <- Option(release.requiresPython)
      } yield value

      getVulnerabilities(dependency).map(vulnerabilities => {
        PackageDetails(
          Some(latestVersion),
          vulnerabilities.map(_.id),
          requiredPython
        )
      })

    })

  private def getLatestDependencyInfo(
      dependency: Dependency
  ): Try[PypiResponse] = Try {
    val response =
      requests.get(s"https://pypi.org/pypi/${dependency.name}/json")
    Utils.JSON.parse[PypiResponse](response.text())
  }
}
