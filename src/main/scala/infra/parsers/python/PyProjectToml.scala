package infra.parsers.python

import scala.collection.JavaConverters.*
import scala.util.{ Success, Try }

import cats.*
import cats.implicits.*
import com.moandjiezana.toml.Toml
import core.domain.dependency.Dependency

object PyProjectToml:
  def extract(groupName: Option[String])(
      fileContents: String
  ): Try[List[Dependency]] =
    val toml = new Toml().read(fileContents)
    groupName
      .fold(extract(toml))(extractSpecificGroupOnly(toml))
      .map(_.filter(_.name != "python"))

  private def extractSpecificGroupOnly(toml: Toml)(
      groupName: String
  ): Try[List[Dependency]] =
    parseDependencies(toml, groupName)

  private def extract(toml: Toml): Try[List[Dependency]] =
    toml.entrySet.asScala.foldLeft(Try(List.empty[Dependency]))((acc, entry) =>
      val key = entry.getKey
      if key.endsWith("dependencies") then
        parseDependencies(toml, key).flatMap(dependencies =>
          acc.map(dependencies ::: _)
        )
      else
        acc.map(
          _ ::: Try(entry.getValue.asInstanceOf[Toml])
            .flatMap(extract)
            .getOrElse(List.empty)
        )
    )

  private def parseDependencies(
      toml: Toml,
      key: String
  ): Try[List[Dependency]] = Try(
    toml
      .getTable(key)
      .toMap
      .asScala
      .map((name, version) =>
        Dependency(name, Option(version).map(v => normalize(v.toString)))
      )
      .toList
  )

  private def normalize(version: String): String = version match
    case s"""${_}version=$v,""" => v
    case s"""${_}version=$v}""" => v
    case other                  => other
