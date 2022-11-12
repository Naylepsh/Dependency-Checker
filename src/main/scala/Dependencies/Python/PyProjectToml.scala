package Dependencies.Python

import Dependencies.Dependency
import Dependencies.Utils.ltrim
import com.moandjiezana.toml.Toml
import scala.util.matching.Regex
import scala.collection.JavaConverters._
import scala.util.Try

object PyProjectToml extends DependencyFormat {

  def parse(fileContents: String): List[Dependency] =
    val toml = new Toml().read(fileContents)
    parseDependencies(toml, dependenciesSectionName).getOrElse(List())

  private def parseDependencies(
      toml: Toml,
      key: String
  ): Try[List[Dependency]] = Try(
    toml
      .getTable(key)
      .toMap
      .asScala
      .map { case (name, version) =>
        Dependency(
          name = name,
          currentVersion = Option(version).map(_.toString),
          latestVersion = None,
          vulnerabilities = List(),
          notes = None
        )
      }
      .toList
  )

  private val dependenciesSectionName = "tool.poetry.dependencies"

}
