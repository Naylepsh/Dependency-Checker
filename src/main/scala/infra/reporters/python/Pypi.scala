package infra.reporters.python

import upickle.default.{ReadWriter => RW, macroRW}
import scala.util.Try
import domain.dependency.Dependency
import domain.dependency.DependencyDetails
import infra.json

object Pypi:
  case class PackageInfo(version: String)
  object PackageInfo:
    given RW[PackageInfo] = macroRW

  case class PackageRelease(
      @upickle.implicits.key("upload_time")
      uploadTime: String,
      @upickle.implicits.key("requires_python")
      requiresPython: String
  )
  object PackageRelease:
    given RW[PackageRelease] = macroRW

  case class PackageVulnerability(id: String, details: String)
  object PackageVulnerability:
    given RW[PackageVulnerability] = macroRW

  case class PypiResponse(
      info: PackageInfo,
      releases: Map[String, List[PackageRelease]]
  )
  object PypiResponse:
    given RW[PypiResponse] = macroRW

  case class VulnerabilitiesResponse(
      vulnerabilities: List[PackageVulnerability]
  )
  object VulnerabilitiesResponse:
    given RW[VulnerabilitiesResponse] = macroRW

  def getDependencyDetails(dependency: Dependency): Try[DependencyDetails] =
    getLatestDependencyInfo(dependency).flatMap(response =>
      val latestVersion = response.info.version
      val requiredPython = for
        release <- response.releases
          .get(latestVersion)
          .flatMap(_.headOption)
        value <- Option(release.requiresPython)
      yield value

      getVulnerabilities(dependency).map(vulnerabilities => {
        DependencyDetails(
          dependency.name,
          dependency.currentVersion.getOrElse(latestVersion),
          latestVersion,
          vulnerabilities.map(_.id),
          requiredPython
        )
      })
    )

  private def getLatestDependencyInfo(
      dependency: Dependency
  ): Try[PypiResponse] = Try {
    val response =
      requests.get(s"https://pypi.org/pypi/${dependency.name}/json")
    json.parse[PypiResponse](response.text())
  }

  private def getVulnerabilities(
      dependency: Dependency
  ): Try[List[PackageVulnerability]] = Try {
    val resource = dependency.currentVersion match
      case Some(version) => s"${dependency.name}/${cleanupVersion(version)}"
      case None          => dependency.name

    val response =
      requests.get(s"https://pypi.org/pypi/$resource/json")
    json.parse[VulnerabilitiesResponse](response.text()).vulnerabilities
  }

  private def cleanupVersion(version: String): String =
    version
      // This is a temporary hack, for ~/^ version shoud be bumped to the latest appropriate one
      .replaceAll("[\\^~]", "")
      // Another hack, * should take the latest available version, not 0
      .replaceAll("\\*", "0")
