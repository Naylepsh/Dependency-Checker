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
      None
    else
      line.filterNot(_.isWhitespace).split("=", 2).toList match {
        case Nil => None

        case name :: Nil =>
          dependencyNamePattern
            .findFirstIn(name)
            .map(cleanName => {
              Dependency(
                name = cleanName,
                currentVersion = None,
                latestVersion = None,
                vulnerabilities = List(),
                notes = None
              )
            })

        case name :: currentVersion :: _ =>
          dependencyNamePattern
            .findFirstIn(name)
            .flatMap(cleanName => {
              dependencyVersionPattern
                .findFirstIn(currentVersion)
                .map(cleanVersion => {
                  Dependency(
                    name = cleanName,
                    currentVersion = Some(cleanVersion),
                    latestVersion = None,
                    vulnerabilities = List(),
                    notes = None
                  )
                })
            })
      }
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

  private val dependencyNamePattern: Regex =
    "[-_a-zA-Z0-9]+".r

  private val dependencyVersionPattern: Regex =
    "[-^~._a-zA-Z0-9]+".r
}
