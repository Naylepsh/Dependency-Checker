package Dependencies.Python

import Dependencies.Dependency
import Dependencies.Utils.ltrim
import scala.util.matching.Regex

object RequirementsTxt extends DependencyFormat {

  def parse(fileContents: String): List[Dependency] = {
    fileContents.split("\n").flatMap(ltrim andThen parseLine).toList
  }

  private def parseLine(line: String): Option[Dependency] = {
    // Will most likely benefit from using a parser?
    if (line.startsWith("#") || line.contains("git"))
      None
    else
      line.split("==", 2).toList match {
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

  private val dependencyNamePattern: Regex =
    "[-_a-zA-Z0-9]+".r

  private val dependencyVersionPattern: Regex =
    "[-._a-zA-Z0-9]+".r
}
