package dependencies

import scala.util.Try
import scala.util.matching.Regex
import scala.util.Success

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

    def parseDependencyVersions(versionsText: String): List[String] =
      versionPattern.findAllIn(versionsText).toList
  }

  def getLatestVersion(versions: List[String]): Option[String] =
    versions.sorted(Ordering.String.reverse) match {
      case Nil => None
      case xs  => Some(xs.head)
    }

  def fillInDependency(
      parseDependencyVersions: String => List[String]
  )(dependency: Dependency): Dependency =
    dependency.copy(latestVersion =
      getLatestVersion(parseDependencyVersions(dependency.name))
    )

  private def ltrim(s: String): String = s.replaceAll("^\\s+", "")

  private def convertToOption[T](value: T): Option[T] =
    if (value != null) Some(value) else None

  private val dependencyPattern: Regex = "([a-zA-Z0-9]+)(==)?(.+)?".r
}
