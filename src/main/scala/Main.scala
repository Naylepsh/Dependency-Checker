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
import Dependencies.RepositoryDependencies
import Dependencies.Excel
import Dependencies.RepositoryDependenciesSheetExporter

@main def readRemote: Unit =
  import Data._

  val content = Source.fromFile("./registry.json").getLines.mkString("\n")
  val registry = JSON.parse[Registry](content)

  val gitlabProps = GitlabProps(registry.host, Some(registry.token))
  val props = ProjectDependenciesFileProps(gitlabProps)
  val resultsFuture = Future.sequence(
    registry.projects.map(project => {
      val file = Future { Gitlab.getProjectDependenciesFile(props)(project.id) }
      val dependencies = file.flatMap {
        case Success(fileOption) =>
          fileOption match {
            case Some(file) =>
              Python.getDependencies(file, Python.Pypi.getDependencyDetails)
            case None => Future { List[Dependency]() }
          }
        case Failure(error) => Future { List[Dependency]() }
      }
      dependencies.map(dependencies =>
        RepositoryDependencies(project.name, dependencies)
      )
    })
  )

  val dependencies = Await.result(resultsFuture, Duration.Inf)
  val workBook =
    Excel.createWorkbook(dependencies)(RepositoryDependenciesSheetExporter())
  Excel.saveWorkbook(workBook, "./export.xlsx")

def showResultsInConsole(repoDependencies: RepositoryDependencies): Unit = {
  println("*" * 10)
  println(repoDependencies.name)
  repoDependencies.dependencies.foreach(println)
}

object Data {
  case class Project(id: String, name: String)
  object Project {
    implicit val rw: RW[Project] = macroRW
  }

  case class Registry(
      host: String,
      token: String,
      projects: List[Project]
  )
  object Registry {
    implicit val rw: RW[Registry] = macroRW
  }
}
