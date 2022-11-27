package services.sources

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}
import cats._
import cats.implicits._
import org.legogroup.woof.{given, *}
import domain.dependency._
import domain.registry._
import services.GitlabApi
import services.sources.python._

object GitlabSource {
  case class GitlabProps(host: String, token: Option[String])

  def make[F[_]: Monad: Logger](
      api: GitlabApi[F],
      contentParser: Format => String => List[Dependency] = defaultContentParser
  ): Source[F, Project] =
    import services.responses._

    new Source[F, Project] {
      def extract(project: Project): F[List[Dependency]] =
        project.sources
          .traverse { case DependencySource(path, format) =>
            extractFromFile(project, path, contentParser(format))
          }
          .map(_.flatten)

      private def extractFromFile(
          project: Project,
          filePath: String,
          contentExtractor: String => List[Dependency]
      ): F[List[Dependency]] = {
        api
          .getFile(project.id, project.branch, filePath)
          .flatMap(_ match
            case Left(reason) => {
              Logger[F].error(
                s"Could not get the file contents of ${project.name} and $filePath due to $reason"
              ) *> List.empty.pure
            }

            case Right(RepositoryFile(content)) =>
              decodeContent(content) match
                case Left(_) => {
                  Logger[F].error(
                    s"Could not decode content of ${project.name}'s $filePath"
                  ) *> List.empty.pure
                }

                case Right(decodedContent) =>
                  contentExtractor(decodedContent).pure
          )
      }
    }

  private def find[A, B, C](
      xs: List[A],
      ys: Map[B, C],
      f: A => B
  ): Option[(A, C)] = {
    // Find first instance of (x, ys[f(x)]) such that f(x) is in ys
    xs
      .find(x => ys.contains(f(x)))
      .flatMap(x => ys.get(f(x)).map(y => (x, y)))
  }

  private def decodeContent(encodedContent: String): Either[Throwable, String] =
    Try(new String(java.util.Base64.getDecoder.decode(encodedContent))).toEither

  private def defaultContentParser(format: Format): String => List[Dependency] =
    format match
      case Format.Txt => RequirementsTxt.extract
      case Format.TOML =>
        PyProjectToml.extract.andThen(_.getOrElse(List.empty))

}
