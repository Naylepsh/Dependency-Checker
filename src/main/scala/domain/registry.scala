package domain

import scala.util.control.NoStackTrace

import cats.*
import cats.implicits.*
import io.circe.syntax.*
import io.circe.*
import io.circe.{ Decoder, HCursor }

object registry:
  sealed trait DependencySource:
    val path: String
    val groupName: String
  object DependencySource:
    case class TxtSource(path: String) extends DependencySource derives Decoder:
      val groupName: String = path

    case class TomlSource(path: String, group: Option[String] = None)
        extends DependencySource derives Decoder:
      val groupName: String = group.fold(path)(g => s"$path ($g)")

    given Decoder[DependencySource] = new Decoder[DependencySource]:
      final def apply(c: HCursor): Decoder.Result[DependencySource] =
        // poor man's ADT discriminator impl
        c.downField("type").as[String].flatMap {
          t =>
            t match
              case "txt"  => c.as[TxtSource]
              case "toml" => c.as[TomlSource]
              case _ => Left(DecodingFailure(
                  DecodingFailure.Reason.CustomReason(
                    s"Unexpected format type $t"
                  ),
                  List.empty
                ))
        }

  case class Project(
      id: String,
      name: String,
      sources: List[DependencySource],
      enabled: Boolean = true,
      branch: String = "master"
  ) derives Decoder

  case class Registry(
      host: String,
      token: String,
      projects: List[Project]
  ) derives Decoder

  trait RegistryRepository[F[_]]:
    def get(): F[Either[Throwable, Registry]]
