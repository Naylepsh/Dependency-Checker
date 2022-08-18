package Dependencies

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}
import upickle.default.{ReadWriter => RW, macroRW}

import Utils.JSON._

object Gitlab {
  case class GitlabProps(host: String, token: Option[String])

  case class RepositoryTreeFile(name: String, path: String)
  object RepositoryTreeFile {
    given RW[RepositoryTreeFile] = macroRW
  }

  case class RepositoryFile(content: String)
  object RepositoryFile {
    given RW[RepositoryFile] = macroRW
  }

  type RepositoryTree = List[RepositoryTreeFile]

  // TBD: should dependencyFileCandidates be passable instead of being hard-coded?
  case class ProjectDependenciesFileProps(
      getProjectTree: String => Try[RepositoryTree],
      getProjectFile: String => String => Try[RepositoryFile]
  )

  object ProjectDependenciesFileProps {
    def apply(gitlabProps: GitlabProps): ProjectDependenciesFileProps =
      ProjectDependenciesFileProps(
        getProjectTree(gitlabProps),
        getProjectFile(gitlabProps)
      )
  }

  def getProjectDependenciesFile(
      props: ProjectDependenciesFileProps
  )(projectId: String): Try[Option[DependencyFile]] = {
    props
      .getProjectTree(projectId)
      .flatMap(tree => {
        val dependencyFileOption = tree
          .find(file => dependencyFileCandidates.contains(file.name))

        dependencyFileOption match {
          case None => Try { None }
          case Some(treeFile) =>
            props
              .getProjectFile(projectId)(treeFile.path)
              .map(file =>
                Some(
                  DependencyFile(
                    content = decodeFile(file.content),
                    format = dependencyFileCandidates(treeFile.name)
                  )
                )
              )
        }
      })
  }

  private def getProjectTree(
      props: GitlabProps
  )(projectId: String): Try[RepositoryTree] = Try {
    // Gitlab defaults to 20 (and paginate to get more).
    // Using 100 to give some safety net for pagination-less result
    val filesPerPage = 100
    val response = requests.get(
      s"https://${props.host}/api/v4/projects/$projectId/repository/tree",
      params = Map(
        "ref" -> "master",
        "private_token" -> props.token.getOrElse(""),
        "per_page" -> filesPerPage.toString
      )
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

  private val dependencyFileCandidates = Map(
    "requirements.txt" -> Txt,
    "pyproject.toml" -> Toml
  )

}
