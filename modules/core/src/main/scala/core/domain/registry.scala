package core.domain

import scala.util.control.NoStackTrace

import cats.*
import cats.implicits.*
import io.circe.*
import io.circe.syntax.*

object registry:
  // TODO: delete this entire package
  sealed trait DependencySource:
    val path: String
    val groupName: String
  object DependencySource:
    case class TxtSource(path: String) extends DependencySource derives Decoder:
      val groupName: String = path

    case class TomlSource(path: String, group: Option[String] = None)
        extends DependencySource derives Decoder:
      val groupName: String = group.fold(path)(g => s"$path:$g")

    given Decoder[DependencySource] with
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

  private case class RawProjectScanConfig(
      id: String,
      name: String,
      sources: List[DependencySource],
      enabled: Option[Boolean],
      branch: Option[String]
  ) derives Decoder

  case class ProjectScanConfig(
      id: String,
      name: String,
      sources: List[DependencySource],
      enabled: Boolean,
      branch: String
  )
  object ProjectScanConfig:
    given Decoder[ProjectScanConfig] = Decoder[RawProjectScanConfig].map(raw =>
      ProjectScanConfig(
        raw.id,
        raw.name,
        raw.sources,
        raw.enabled.getOrElse(true),
        raw.branch.getOrElse("master")
      )
    )

  case class Registry(projects: List[ProjectScanConfig]) derives Decoder
  object Registry:
    val empty: Registry = Registry(projects = List.empty)

  trait RegistryRepository[F[_]]:
    def get(): F[Either[Throwable, Registry]]
