package domain

import upickle.default.{ReadWriter => RW, macroRW, readwriter}
import cats._
import cats.implicits._
import scala.util.control.NoStackTrace

object registry:
  sealed trait DependencySource:
    val path: String
  object DependencySource:
    case class TxtSource(path: String) extends DependencySource
    object TxtSource:
      given RW[TxtSource] = macroRW

    case class TomlSource(path: String, group: Option[String] = None)
        extends DependencySource
    object TomlSource:
      given RW[TomlSource] = macroRW

    given RW[DependencySource] = macroRW

  case class Project(
      id: String,
      name: String,
      sources: List[DependencySource],
      enabled: Boolean = true,
      branch: String = "master"
  )
  object Project {
    given RW[Project] = macroRW
  }

  case class Registry(
      host: String,
      token: String,
      projects: List[Project]
  )
  object Registry {
    given RW[Registry] = macroRW
  }
