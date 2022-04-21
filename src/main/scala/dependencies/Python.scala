package dependencies

import scala.util.Try
import scala.util.matching.Regex
import scala.util.Success
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

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
    /**
     * Can be replaced by requests to pypi.org/pypi/<lib-name>/json
     */
    val versionPattern = "[0-9]+.[.0-9a-zA-Z]+".r
    val versionLinesPattern = "ERROR:[^\n]*\n".r

    def parseDependencyVersions(versionsText: String): List[String] = {
      versionPattern.findAllIn(versionsText).toList
    }

    def getDependencyVersions(name: String): List[String] = {
      val pipOutput =  os
        .proc("pip", "install", s"$name==")
        .spawn(stderr = os.Pipe)
        .stderr
        .lines()
        .mkString("\n")
      
      val versionsLine = versionLinesPattern.findFirstIn(pipOutput).getOrElse("")
      parseDependencyVersions(versionsLine)
    }
  }

  def getLatestVersion(versions: List[String]): Option[String] =
    versions.sorted(Ordering.String.reverse) match {
      case Nil => None
      case xs  => Some(xs.head)
    }

  def fillInDependency(
      getDependencyVersions: String => List[String]
  )(dependency: Dependency): Dependency =
    dependency.copy(latestVersion =
      getLatestVersion(getDependencyVersions(dependency.name))
    )

  def getDependencies(
      fileContents: String,
      getDependencyVersions: String => List[String]
  )(implicit ec: ExecutionContext): Future[List[Dependency]] = {
    val dependenciesFutures = parseRequirements(fileContents).map(dependency =>
      Future { fillInDependency(getDependencyVersions)(dependency) }
    )
    Future.sequence(dependenciesFutures)
  }

  private def ltrim(s: String): String = s.replaceAll("^\\s+", "")

  private def convertToOption[T](value: T): Option[T] =
    if (value != null) Some(value) else None

  private val dependencyPattern: Regex = "([-a-zA-Z0-9]+)(==)?(.+)?".r
}
