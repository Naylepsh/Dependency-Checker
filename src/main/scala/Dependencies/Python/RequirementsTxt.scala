package Dependencies.Python

import Dependencies.Dependency
import scala.util.matching.Regex
import Dependencies.Utils.ltrim

object RequirementsTxt extends DependencyFormat {
  def parse(fileContents: String): List[Dependency] = {
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
          latestVersion = None
        )
      })
  }

  private val dependencyPattern: Regex = "([-_a-zA-Z0-9]+)(==)?(.+)?".r
}
