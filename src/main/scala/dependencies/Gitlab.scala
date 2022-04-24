package dependencies

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}
import upickle.default.{ReadWriter => RW, macroRW}

import Utils.JSON._

object Gitlab {
  case class GitlabProps(host: String, token: Option[String])

  case class RepositoryTreeFile(name: String, path: String)
  object RepositoryTreeFile {
    implicit val rw: RW[RepositoryTreeFile] = macroRW
  }

  case class RepositoryFile(content: String)
  object RepositoryFile {
    implicit val rw: RW[RepositoryFile] = macroRW
  }

  type RepositoryTree = List[RepositoryTreeFile]

  def getProjectDependenciesTreeFile(props: GitlabProps)(
      projectId: String
  )(implicit ec: ExecutionContext): Future[Try[String]] = Future {
    getProjectTree(props)(projectId).flatMap(tree => {
      // TODO: handle the case where no file matches the candidates (no head present)
      val dependencyFilePath = tree
        .filter(file => dependencyFileCandidates.contains(file.name))
        .head
      getProjectFile(props)(projectId)(dependencyFilePath.path).map(file =>
        decodeFile(file.content)
      )
    })
  }

  private def getProjectTree(
      props: GitlabProps
  )(projectId: String): Try[RepositoryTree] = Try {
    val response = requests.get(
      s"https://${props.host}/api/v4/projects/$projectId/repository/tree",
      params =
        Map("ref" -> "master", "private_token" -> props.token.getOrElse(""))
    )
    parse[RepositoryTree](response.text())
  }

  private def getProjectFile(props: GitlabProps)(projectId: String)(
      path: String
  ): Try[RepositoryFile] = Try {
    val response = requests.get(
      s"https://${props.host}/api/v4/projects/$projectId/repository/files/$path",
      params =
        Map("ref" -> "master", "private_token" -> props.token.getOrElse(""))
    )
    parse[RepositoryFile](response.text())
  }

  private def decodeFile(encodedContent: String): String =
    new String(java.util.Base64.getDecoder.decode(encodedContent))

  private val dependencyFileCandidates = List("requirements.txt")

}
