package domain

import scala.util.control.NoStackTrace

import cats.*
import cats.implicits.*
import upickle.default.{ ReadWriter as RW, macroRW, readwriter }

object registry:
  sealed trait DependencySource:
    val path: String
    val groupName: String
  object DependencySource:
    case class TxtSource(path: String) extends DependencySource:
      val groupName: String = path
    object TxtSource:
      given RW[TxtSource] = macroRW

    case class TomlSource(path: String, group: Option[String] = None)
        extends DependencySource:
      val groupName: String = group.fold(path)(g => s"$path ($g)")
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
  object Project:
    given RW[Project] = macroRW

  case class Registry(
      host: String,
      token: String,
      projects: List[Project]
  )
  object Registry:
    given RW[Registry] = macroRW

  trait RegistryRepository[F[_]]:
    def get(): F[Registry]
