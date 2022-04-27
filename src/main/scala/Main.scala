import scala.util.{Success, Failure, Try}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import upickle.default.{ReadWriter => RW, macroRW}

import Dependencies.Python
import Dependencies.Dependency
import Dependencies.Utils.JSON
import Dependencies.Gitlab
import Dependencies.Gitlab.GitlabProps
import Dependencies.Gitlab.ProjectDependenciesFileProps

def readLocal: Unit =
  val repoPaths = List(
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-core",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-lynx",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-delegator",
    "/Users/sebastian.kondraciuk/Documents/code/vsa/vsa-pydentifier"
  )

  def appendReqsFile(path: String): String = s"$path/requirements.txt"
  def getLocalFileContents(path: String): String =
    Source.fromFile(path).getLines.mkString("\n")

  val getDeps = Python.getDependencies(
    x => Future { appendReqsFile.andThen(getLocalFileContents)(x) },
    Python.Pypi.getLatestVersion
  )

  val dependenciesFuture =
    Future.sequence(repoPaths.map(path => Future { (path, getDeps(path)) }))

  val dependencies = Await.result(dependenciesFuture, Duration.Inf)
  dependencies.map {
    case (path, dependenciesFuture) => {
      dependenciesFuture.map { dependencies =>
        println("*" * 10)
        println(path)
        dependencies.foreach(println)
      }
    }
  }

@main def readRemote: Unit =
  import Data._

  val content = Source.fromFile("./registry.json").getLines.mkString("\n")
  val registry = JSON.parse[Registry](content)

  val gitlabProps = GitlabProps(registry.host, Some(registry.token))
  val props = ProjectDependenciesFileProps(gitlabProps)
  val resultsFuture = Future.sequence(
    registry.projectIds.map(id =>
      Gitlab.getProjectDependenciesFile(props)(id) match {
        case Success(fileOption) =>
          fileOption match {
            case Some(file) =>
              Python.getDependencies(file, Python.Pypi.getLatestVersion)
            case None => Future { List[Dependency]() }
          }
        case Failure(error) => Future { List[Dependency]() }
      }
    )
  )

  val dependencies = Await.result(resultsFuture, Duration.Inf)
  dependencies.map { repoDependencies =>
    {
      println("*" * 10)
      repoDependencies.map { dependencies =>
        println(dependencies)
      }
    }
  }

object Data {
  case class Registry(
      host: String,
      token: String,
      projectIds: List[String]
  )
  object Registry {
    implicit val rw: RW[Registry] = macroRW
  }
}
