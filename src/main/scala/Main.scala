import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent._
import cats._
import cats.implicits._
import services.DependencyService
import services.sources.GitlabSource
import services.reporters.python.PythonDependencyReporter
import services.exports.ConsoleExporter
import services.exports.ExcelExporter
import services.sources.GitlabSource.ProjectProps

@main
def app: Unit = {
  import utils._
  import domain.registry._

  val exportDestination = "./export.xslx"
  val registrySource = "./registry.json"
  val content = Source.fromFile(registrySource).getLines.mkString("\n")
  val registry = json.parse[Registry](content)

  def prepareForSource(
      project: domain.project.Project
  ): Option[ProjectProps] =
    registry.projects
      .find(_.id == project.id)
      .map(project => ProjectProps(project.id, project.branch))

  val service =
    DependencyService.make[Future, ProjectProps](
      source = GitlabSource.forFuture(
        GitlabSource.GitlabProps(registry.host, registry.token.some)
      ),
      prepareForSource = prepareForSource,
      reporter = PythonDependencyReporter.forFuture,
      exporter =
        ExcelExporter.make(ExcelExporter.dependencies.toSheet, registrySource)
    )

  Await.result(
    service.checkDependencies(registry.projects.map {
      case Project(id, name, branch) => domain.project.Project(id, name)
    }),
    Duration.Inf
  )
}
