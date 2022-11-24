package domain

import upickle.default.{ReadWriter => RW, macroRW, readwriter}
import cats._
import cats.implicits._
import util.Try
import scala.util.control.NoStackTrace

object registry {
  enum Format:
    case Txt, TOML
  object Format:
    given RW[Format] = readwriter[String].bimap(
      _.toString,
      _.toLowerCase match {
        case "txt"  => Txt
        case "toml" => TOML
        case s      => throw UnregisteredFormat(s)
      }
    )

  case class UnregisteredFormat(format: String) extends NoStackTrace

  case class DependencySource(
      path: String,
      format: Format
  )
  object DependencySource:
    given RW[DependencySource] = macroRW

  case class Project(
      id: String,
      name: String,
      sources: List[DependencySource],
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
}
