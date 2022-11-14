package services.sources

import domain.dependency._
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}
import upickle.default.{ReadWriter => RW, macroRW}
import services.sources.python.RequirementsTxt
import services.sources.python.PyProjectToml
import cats._
import cats.implicits._
import cats.data.EitherT

object GitlabSource {
  object responses {
    case class RepositoryTreeFile(name: String, path: String)
    object RepositoryTreeFile {
      given RW[RepositoryTreeFile] = macroRW
    }

    case class RepositoryFile(content: String)
    object RepositoryFile {
      given RW[RepositoryFile] = macroRW
    }

    type RepositoryTree = List[RepositoryTreeFile]
  }

  case class GitlabProps(host: String, token: Option[String])
  case class ProjectProps(id: String, branch: String)

  import responses._

  def make[F[_]: Monad](
      getProjectTree: ProjectProps => F[RepositoryTree],
      getProjectFile: (ProjectProps, String) => F[RepositoryFile]
  ): Source[F, ProjectProps] =
    new Source[F, ProjectProps] {
      def extract(projectProps: ProjectProps): F[List[Dependency]] =
        for {
          tree <- getProjectTree(projectProps)
          fileCandidate = tree.find(file =>
            dependencyFileCandidates.contains(file.name)
          )
          dependencies <- fileCandidate match
            case None => List.empty.pure
            case Some(file) =>
              getProjectFile(projectProps, file.path).map {
                case RepositoryFile(content) =>
                  dependencyFileCandidates
                    .get(file.name)
                    .map(_(decodeContent(content)))
                    .getOrElse(List.empty)
              }

        } yield dependencies
    }

  def forFuture(
      gitlabProps: GitlabProps
  )(using ExecutionContext): Source[Future, ProjectProps] = {
    val trySource = make[Try](
      getProjectTree(gitlabProps),
      getProjectFile(gitlabProps)
    )

    new Source[Future, ProjectProps] {

      override def extract(src: ProjectProps): Future[List[Dependency]] =
        Future(trySource.extract(src).getOrElse(List.empty))

    }
  }

  private def getProjectTree(
      gitlabProps: GitlabProps
  )(projectProps: ProjectProps): Try[RepositoryTree] = Try {
    // Gitlab defaults to 20 (and paginate to get more).
    // Using 100 to give some safety net for pagination-less result
    val filesPerPage = 100
    val response = requests.get(
      s"https://${gitlabProps.host}/api/v4/projects/${projectProps.id}/repository/tree",
      params = Map(
        "ref" -> projectProps.branch,
        "private_token" -> gitlabProps.token.getOrElse(""),
        "per_page" -> filesPerPage.toString
      )
    )
    parse[RepositoryTree](response.text())
  }

  private def getProjectFile(gitlabProps: GitlabProps)(
      projectProps: ProjectProps,
      path: String
  ): Try[RepositoryFile] = Try {
    val response = requests.get(
      s"https://${gitlabProps.host}/api/v4/projects/${projectProps.id}/repository/files/$path",
      params = Map(
        "ref" -> projectProps.branch,
        "private_token" -> gitlabProps.token.getOrElse("")
      )
    )
    parse[RepositoryFile](response.text())
  }

  private def decodeContent(encodedContent: String): String =
    new String(java.util.Base64.getDecoder.decode(encodedContent))

  private val dependencyFileCandidates = Map(
    "requirements.txt" -> RequirementsTxt.extract,
    "pyproject.toml" -> PyProjectToml.extract.andThen(_.getOrElse(List.empty))
  )

  private def parse[T: RW](jsonString: String): T =
    upickle.default.read[T](jsonString)
}
