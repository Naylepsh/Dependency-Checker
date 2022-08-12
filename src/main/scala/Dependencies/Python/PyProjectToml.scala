package Dependencies.Python

import Dependencies.Dependency
import scala.util.matching.Regex
import Dependencies.Utils.ltrim

object PyProjectToml extends DependencyFormat {
  def parse(fileContents: String): List[Dependency] = {
    getDependenciesPart(fileContents, dependenciesSectionName)
      .getOrElse(List())
      .flatMap(ltrim andThen parseLine)
  }

  private def parseLine(line: String): Option[Dependency] = {
    if (line.startsWith("#") || line.contains("git"))
      return None

    dependencyPattern
      .findFirstMatchIn(line)
      .map(patternMatch => {
        Dependency(
          name = patternMatch.group(1),
          currentVersion = Option(patternMatch.group(4)),
          latestVersion = None
        )
      })
  }

  def getDependenciesPart(
      fileContents: String,
      sectionName: String
  ): Option[List[String]] = {
    // Can't use s"""\[$sectionName\]""" due to compilation errors
    val section = List("""\[""", sectionName, """\]""").mkString

    fileContents
      .split(section)
      .tail
      .headOption
      .map(
        _.split("\n")
          .takeWhile(str => !str.startsWith("["))
          .filter(_.length > 0)
          .toList
      )
  }

  private val dependenciesSectionName = "tool.poetry.dependencies"

  private val dependencyPattern: Regex =
    """([-_a-zA-Z0-9]+).*=.*(")?(.+)?(")?""".r
}
