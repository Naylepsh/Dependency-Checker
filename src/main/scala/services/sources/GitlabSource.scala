package services.sources

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}
import cats._
import cats.implicits._
import domain.dependency._
import services.GitlabApi
import services.sources.python._

object GitlabSource {
  case class GitlabProps(host: String, token: Option[String])
  case class ProjectProps(id: String, branch: String)

  def make[F[_]: Monad](
      api: GitlabApi[F]
  ): Source[F, ProjectProps] =
    import services.responses._

    new Source[F, ProjectProps] {
      def extract(projectProps: ProjectProps): F[List[Dependency]] =
        api
          .getFileTree(projectProps.id, projectProps.branch)
          .flatMap(_ match
            case Left(reason) => {
              println(
                s"Could not get the tree structure of $projectProps due to $reason"
              )
              List.empty.pure
            }

            case Right(tree) => {
              find(tree, dependencyFileCandidates, _.name)
                .map { case (file, extractor) =>
                  extractFromFile(projectProps, file, extractor)
                }
                .getOrElse(List.empty.pure)
            }
          )

      private def extractFromFile(
          projectProps: ProjectProps,
          file: RepositoryTreeFile,
          contentExtractor: String => List[Dependency]
      ): F[List[Dependency]] = {
        api
          .getFile(projectProps.id, projectProps.branch, file.path)
          .map(_ match
            case Left(reason) => {
              println(
                s"Could not get the file contents of $projectProps and $file due to $reason"
              )
              List.empty
            }

            case Right(RepositoryFile(content)) =>
              contentExtractor(decodeContent(content))
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

  private def decodeContent(encodedContent: String): String =
    new String(java.util.Base64.getDecoder.decode(encodedContent))

  private val dependencyFileCandidates = Map(
    "requirements.txt" -> RequirementsTxt.extract,
    "pyproject.toml" -> PyProjectToml.extract.andThen(_.getOrElse(List.empty))
  )
}
