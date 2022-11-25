package services.sources.python

import services.sources._
import domain.dependency.Dependency
import com.moandjiezana.toml.Toml
import scala.util.Try
import scala.collection.JavaConverters._
import cats._
import cats.implicits._
import scala.util.Success

object PyProjectToml {
  def extract(fileContents: String): Try[List[Dependency]] =
    val toml = new Toml().read(fileContents)
    extract(toml)

  private def extract(toml: Toml): Try[List[Dependency]] =
    toml.entrySet.asScala.foldLeft(Try(List.empty[Dependency])) {
      case (acc, entry) =>
        val key = entry.getKey
        if (key.endsWith("dependencies"))
          parseDependencies(toml, key).flatMap(dependencies =>
            acc.map(dependencies ::: _)
          )
        else
          acc.map(
            _ ::: Try(entry.getValue.asInstanceOf[Toml])
              .flatMap(extract)
              .getOrElse(List.empty)
          )
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
}
