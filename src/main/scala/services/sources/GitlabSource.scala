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
import domain.registry.DependencySource.TxtSource
import domain.registry.DependencySource.TomlSource

object GitlabSource:
  case class GitlabProps(host: String, token: Option[String])

  def make[F[_]: Monad: Logger](
      api: GitlabApi[F],
      contentParser: DependencySource => String => List[Dependency] =
        defaultContentParser
  ): Source[F, Project] =
    import services.responses._

    new Source[F, Project] {
      def extract(project: Project): F[List[Dependency]] =
        project.sources
          .traverse(source =>
            extractFromFile(project, source.path, contentParser(source))
          )
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

  private def defaultContentParser(
      source: DependencySource
  ): String => List[Dependency] =
    source match
      case TxtSource(path) => RequirementsTxt.extract
      case TomlSource(path, group) =>
        PyProjectToml.extract(group).andThen(_.getOrElse(List.empty))
