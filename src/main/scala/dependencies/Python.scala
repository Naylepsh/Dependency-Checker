package dependencies

import scala.util.Try
import scala.util.matching.Regex
import scala.util.Success
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import upickle.default.{ReadWriter => RW, macroRW}
import javax.security.auth.login.FailedLoginException

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
          latestVersion = None
        )
      })
  }

  object Pip {
    val versionPattern = "[0-9]+.[.0-9a-zA-Z]+".r
    val versionLinesPattern = "ERROR:[^\n]*\n".r

    def parseDependencyVersions(versionsText: String): List[String] = {
      versionPattern.findAllIn(versionsText).toList
    }

    def getDependencyVersions(name: String): List[String] = {
      val pipOutput = os
        .proc("pip", "install", s"$name==")
        .spawn(stderr = os.Pipe)
        .stderr
        .lines()
        .mkString("\n")

      val versionsLine =
        versionLinesPattern.findFirstIn(pipOutput).getOrElse("")
      parseDependencyVersions(versionsLine)
    }
  }

  object Pypi {
    case class PackageInfo(version: String)
    object PackageInfo {
      implicit val rw: RW[PackageInfo] = macroRW
    }

    case class PypiResponse(info: PackageInfo)
    object PypiResponse {
      implicit val rw: RW[PypiResponse] = macroRW
    }

    def getLatestVersion(packageName: String): Try[String] = Try {
      val response = requests.get(s"https://pypi.org/pypi/$packageName/json")
      upickle.default.read[Pypi.PypiResponse](response.text()).info.version
    }
  }

  def getDependencies(
      getFileContents: String => Future[String],
      getLatestVersion: String => Try[String]
  )(path: String)(implicit ec: ExecutionContext): Future[List[Dependency]] = {
    for {
      fileContents <- getFileContents(path)
      dependencies <- Python.getDependencies(fileContents, getLatestVersion)
    } yield dependencies
  }

  def getDependencies(
      fileContents: String,
      getLatestVersion: String => Try[String]
  )(implicit ec: ExecutionContext): Future[List[Dependency]] = {
    val dependenciesFutures = parseRequirements(fileContents).map(dependency =>
      Future {
        dependency.copy(latestVersion =
          tryToOption(getLatestVersion(dependency.name))
        )
      }
    )
    Future.sequence(dependenciesFutures)
  }

  private def ltrim(s: String): String = s.replaceAll("^\\s+", "")

  private def convertToOption[T](value: T): Option[T] =
    if (value != null) Some(value) else None

  private val dependencyPattern: Regex = "([-a-zA-Z0-9]+)(==)?(.+)?".r

  private def tryToOption[T](tryResult: Try[T]): Option[T] = tryResult match {
    case Success(value) => Some(value)
    case _              => None
  }
}
