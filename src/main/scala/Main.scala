import scala.util.{Success, Failure, Try}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent._
import upickle.default.{ReadWriter => RW, macroRW}
import cats._
import cats.implicits._
import services.DependencyService
import services.sources.GitlabSource
import services.reporters.python.PythonDependencyReporter
import services.exports.ConsoleExporter
import services.exports.ExcelExporter

@main
def app: Unit =
  import Data._
  import utils._

  val content = Source.fromFile("./registry.json").getLines.mkString("\n")
  val registry = json.parse[Registry](content)

  def prepareForSource(
      project: domain.project.Project
  ): Option[services.sources.GitlabSource.ProjectProps] =
    registry.projects
      .find(_.id == project.id)
      .map(project =>
        services.sources.GitlabSource.ProjectProps(project.id, project.branch)
      )

  val service =
    DependencyService.make[Future, services.sources.GitlabSource.ProjectProps](
      source = GitlabSource.forFuture(
        GitlabSource.GitlabProps(registry.host, registry.token.some)
      ),
      prepareForSource = prepareForSource,
      reporter = PythonDependencyReporter.forFuture,
      exporter =
        ExcelExporter.make(ExcelExporter.dependencies.toSheet, "./export.xlsx")
    )

  Await.result(
    service.checkDependencies(registry.projects.map {
      case Project(id, name, branch) => domain.project.Project(id, name)
    }),
    Duration.Inf
  )

object Data {
  case class Project(
      id: String,
      name: String,
      branch: String = "master"
  )
  object Project {
    given RW[Project] = macroRW
  }

  case class Registry(
      host: String,
      token: String,
      projects: List[Project]
  )
  object Registry {
    given RW[Registry] = macroRW
  }
}
