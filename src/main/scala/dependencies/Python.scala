package dependencies

import scala.util.Try
import scala.util.matching.Regex

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

  def getDependencyVersions(name: String): Try[List[String]] = ???

  def getLatestVersion(name: String): Try[String] = ???

  private def ltrim(s: String): String = s.replaceAll("^\\s+", "")

  private def convertToOption[T](value: T): Option[T] =
    if (value != null) Some(value) else None

  private val dependencyPattern: Regex = "([a-zA-Z0-9]+)(==)?(.+)?".r
}
