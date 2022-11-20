package services.sources.python

import services.sources._
import domain.dependency.Dependency
import com.moandjiezana.toml.Toml
import scala.util.Try
import scala.collection.JavaConverters._
import cats._
import cats.implicits._

object PyProjectToml {
  def extract(fileContents: String): Try[List[Dependency]] = {
    val toml = new Toml().read(fileContents)
    parseDependencies(toml, dependenciesSectionName)
  }

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
          currentVersion = Option(version).map(_.toString)
        )
      }
      .toList
  )

  private val dependenciesSectionName = "tool.poetry.dependencies"
}
