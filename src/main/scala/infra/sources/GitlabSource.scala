package infra.sources

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

import cats.*
import cats.implicits.*
import domain.Source
import domain.dependency.*
import domain.project.Grouped
import domain.registry.DependencySource.{TomlSource, TxtSource}
import domain.registry.*
import infra.GitlabApi
import infra.parsers.python.{PyProjectToml, RequirementsTxt}
import org.legogroup.woof.{ *, given }

object GitlabSource:
  case class GitlabProps(host: String, token: Option[String])

  def make[F[_]: Monad: Logger](
      api: GitlabApi[F],
      contentParser: DependencySource => String => List[Dependency] =
        defaultContentParser
  ): Source[F, Project] =
    import infra.responses.*

    new Source[F, Project]:
      def extract(project: Project): F[List[Grouped[Dependency]]] =
        project.sources
          .traverse(source =>
            extractFromFile(project, source.path, contentParser(source)).map(dependencies =>
              Grouped(source.groupName, dependencies)
            )
          )

      private def extractFromFile(
          project: Project,
          filePath: String,
          contentExtractor: String => List[Dependency]
      ): F[List[Dependency]] =
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

  private def find[A, B, C](
      xs: List[A],
      ys: Map[B, C],
      f: A => B
  ): Option[(A, C)] =
    // Find first instance of (x, ys[f(x)]) such that f(x) is in ys
    xs
      .find(x => ys.contains(f(x)))
      .flatMap(x => ys.get(f(x)).map(y => (x, y)))

  private def decodeContent(encodedContent: String): Either[Throwable, String] =
    Try(new String(java.util.Base64.getDecoder.decode(encodedContent))).toEither

  private def defaultContentParser(
      source: DependencySource
  ): String => List[Dependency] =
    source match
      case TxtSource(path) => RequirementsTxt.extract
      case TomlSource(path, group) =>
        PyProjectToml.extract(group).andThen(_.getOrElse(List.empty))
