package Dependencies

import scala.util.Try
import scala.util.matching.Regex
import scala.util.Success
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
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
          currentVersion = convertToOption(patternMatch.group(3)),
          latestVersion = None,
          vulnerabilities = List(),
          notes = None
        )
      })
  }

  case class PackageDetails(
      latestVersion: Option[String],
      vulnerabilities: List[String]
  ) {
    // Things that are generally unknown when just parsing the local file
  }

  object Pypi {
    case class PackageInfo(version: String)
    object PackageInfo {
      implicit val rw: RW[PackageInfo] = macroRW
    }

    case class PackageRelease(url: String)
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

    def getDependencyDetails(dependency: Dependency): Try[PackageDetails] =
      Try {
        val resource = dependency.currentVersion match {
          case Some(version) => s"${dependency.name}/${version}"
          case None          => dependency.name
        }

        val response = requests.get(s"https://pypi.org/pypi/$resource/json")
        val parsedResponse = Utils.JSON.parse[PypiResponse](response.text())

        val latestRelease =
          parsedResponse.releases.keySet.toList.sorted.lastOption

        PackageDetails(
          latestRelease,
          parsedResponse.vulnerabilities.map(_.id)
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
              vulnerabilities = details.vulnerabilities
            )
          })
          .getOrElse(dependency)
      }
    )
    Future.sequence(dependenciesFutures)
  }

  private def ltrim(s: String): String = s.replaceAll("^\\s+", "")

  private def convertToOption[T](value: T): Option[T] =
    if (value != null) Some(value) else None

  private val dependencyPattern: Regex = "([-_a-zA-Z0-9]+)(==)?(.+)?".r

  private def tryToOption[T](tryResult: Try[T]): Option[T] = tryResult match {
    case Success(value) => Some(value)
    case _              => None
  }
}
