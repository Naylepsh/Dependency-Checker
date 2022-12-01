package services.sources.python

import domain.dependency.Dependency
import scala.util.matching.Regex

object RequirementsTxt {
  def extract(fileContents: String): List[Dependency] = {
    fileContents.split("\n").flatMap(ltrim andThen parseLine).toList
  }

  private def parseLine(line: String): Option[Dependency] = {
    val cleanedLine = preProcess(line)

    if (shouldIgnore(cleanedLine))
      None
    else
      cleanedLine.split("==", 2).toList match {
        case Nil => None

        case name :: Nil =>
          dependencyNamePattern
            .findFirstIn(name)
            .map(cleanName => {
              Dependency(
                name = cleanName,
                currentVersion = None
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
                    currentVersion = Some(cleanVersion)
                  )
                })
            })
      }
  }

  private def preProcess(line: String): String =
    if (line.startsWith("-e"))
      line.replaceFirst("-e", "")
    else
      line

  private def shouldIgnore(line: String): Boolean =
    line.startsWith("#") || line.contains("git+")

  private val dependencyNamePattern: Regex =
    "[-._a-zA-Z0-9]+".r

  private val dependencyVersionPattern: Regex =
    "[-*._a-zA-Z0-9]+".r

  private def ltrim(s: String): String = s.replaceAll("^\\s+", "")
}
