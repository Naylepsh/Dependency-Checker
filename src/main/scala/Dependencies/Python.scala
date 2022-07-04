package Dependencies

import scala.util.{Try, Success}
import scala.util.matching.Regex
import scala.concurrent.{Future, ExecutionContext}
import upickle.default.{ReadWriter => RW, macroRW}

object Python {

  def parseRequirements(fileContents: String): List[Dependency] = {
    fileContents.split("\n").flatMap(ltrim andThen parseRequirementsLine).toList
  }

  private def parseRequirementsLine(line: String): Option[Dependency] = {
    // Will most likely benefit from using a parser?
    if (line.startsWith("#") || line.contains("git"))
      return None

    dependencyPattern
      .findFirstMatchIn(line)
      .map(patternMatch => {
        Dependency(
          name = patternMatch.group(1),
          currentVersion = Option(patternMatch.group(3)),
          latestVersion = None,
          vulnerabilities = List(),
          notes = None
        )
      })
  }

  case class PackageDetails(
      latestVersion: Option[String],
      vulnerabilities: List[String],
      requiredPython: Option[String]
  ) {
    // Things that are generally unknown when just parsing the local file
  }

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
        releases: Map[String, List[PackageRelease]],
        vulnerabilities: List[PackageVulnerability]
    )
    object PypiResponse {
      implicit val rw: RW[PypiResponse] = macroRW
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

    def getDependencyDetails(dependency: Dependency): Try[PackageDetails] =
      Try {
        val resource = dependency.currentVersion match {
          case Some(version) => s"${dependency.name}/$version"
          case None          => dependency.name
        }

        val response = requests.get(s"https://pypi.org/pypi/$resource/json")
        val parsedResponse = Utils.JSON.parse[PypiResponse](response.text())

        val latestVersion = getLatestVersion(parsedResponse)
        val requiredPython = for {
          version <- latestVersion
          release <- parsedResponse.releases.get(version).flatMap(_.headOption)
          value <- Option(release.requiresPython)
        } yield value
        val vulnerabilities = parsedResponse.vulnerabilities.map(_.id)

        PackageDetails(
          latestVersion,
          vulnerabilities,
          requiredPython
        )
      }
  }

  def getDependencies(
      getFileContents: String => Future[String],
      getDependencyDetails: Dependency => Try[PackageDetails]
  )(path: String)(implicit ec: ExecutionContext): Future[List[Dependency]] = {
    for {
      fileContents <- getFileContents(path)
      dependencies <- Python.getDependencies(fileContents, getDependencyDetails)
    } yield dependencies
  }

  def getDependencies(
      fileContents: String,
      getDependencyDetails: Dependency => Try[PackageDetails]
  )(implicit ec: ExecutionContext): Future[List[Dependency]] = {
    val dependenciesFutures = parseRequirements(fileContents).map(dependency =>
      Future {
        getDependencyDetails(dependency)
          .map(details => {
            dependency.copy(
              latestVersion = details.latestVersion,
              vulnerabilities = details.vulnerabilities,
              notes = details.requiredPython.map(version =>
                s"Required python: ${version}"
              )
            )
          })
          .getOrElse(dependency)
      }
    )
    Future.sequence(dependenciesFutures)
  }

  private def ltrim(s: String): String = s.replaceAll("^\\s+", "")

  private val dependencyPattern: Regex = "([-_a-zA-Z0-9]+)(==)?([-._a-zA-Z0-9]+)?".r
}
