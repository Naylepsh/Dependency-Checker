package services.sources

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}
import cats._
import cats.implicits._
import domain.dependency._
import services.GitlabApi
import services.sources.python._
import domain.registry.Project
import domain.registry.DependencySource
import domain.registry.Format

object GitlabSource {
  case class GitlabProps(host: String, token: Option[String])

  def make[F[_]: Monad](
      api: GitlabApi[F],
      contentParser: Format => String => List[Dependency] = defaultContentParser
  ): Source[F, Project] =
    import services.responses._

    new Source[F, Project] {
      def extract(project: Project): F[List[Dependency]] =
        project.sources
          .map { case DependencySource(path, format) =>
            extractFromFile(project, path, contentParser(format))
          }
          .sequence
          .map(_.flatten)

      private def extractFromFile(
          project: Project,
          filePath: String,
          contentExtractor: String => List[Dependency]
      ): F[List[Dependency]] = {
        api
          .getFile(project.id, project.branch, filePath)
          .map(_ match
            case Left(reason) => {
              println(
                s"Could not get the file contents of ${project.name} and $filePath due to $reason"
              )
              List.empty
            }

            case Right(RepositoryFile(content)) =>
              decodeContent(content) match
                case Left(_) => {
                  println(
                    s"Could not decode content of ${project.name}'s $filePath"
                  )
                  List.empty
                }

                case Right(decodedContent) => contentExtractor(decodedContent)
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
