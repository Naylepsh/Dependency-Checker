package dependencies

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}
import upickle.default.{ReadWriter => RW, macroRW}

object Gitlab {
  case class RepositoryTreeFile(name: String, path: String)
  object RepositoryTreeFile {
    implicit val rw: RW[RepositoryTreeFile] = macroRW
  }

  case class RepositoryTree(files: List[RepositoryTreeFile])
  object RepositoryTree {
    implicit val rw: RW[RepositoryTree] = macroRW
  }

  case class RepositoryFile(content: String)
  object RepositoryFile {
    implicit val rw: RW[RepositoryFile] = macroRW
  }

  def getProjectDependenciesTreeFile(host: String)(
      projectId: String
  )(implicit ec: ExecutionContext): Future[Try[String]] = Future {
    getProjectTree(host)(projectId).flatMap(tree => {
      // TODO: handle the case where no file matches the candidates (no head present)
      val dependencyFilePath = tree.files
        .filter(file => dependencyFileCandidates.contains(file.name))
        .head
      getProjectFile(host)(projectId)(dependencyFilePath.path).map(file =>
        decodeFile(file.content)
      )
    })
  }

  private def getProjectTree(
      host: String
  )(projectId: String): Try[RepositoryTree] = Try {
    val response = requests.get(
      s"https://$host/api/v4/projects/$projectId/repository/tree?ref=master"
    )
    parseResponse[RepositoryTree](response.text())
  }

  private def getProjectFile(host: String)(projectId: String)(
      path: String
  ): Try[RepositoryFile] = Try {
    val response = requests.get(
      s"https://$host/api/v4/projects/$projectId/repository/files/$path?ref=master"
    )
    parseResponse[RepositoryFile](response.text())
  }

  def parseResponse[T: RW](responseText: String): T =
    upickle.default.read[T](responseText)

  private def decodeFile(encodedContent: String): String =
    new String(java.util.Base64.getDecoder.decode(encodedContent))

  private val dependencyFileCandidates = List("requirements.txt")

}
