import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent._
import cats._
import cats.implicits._
import services.DependencyService
import services.reporters.python.PythonDependencyReporter
import services.exports.ExcelExporter
import services.GitlabApi
import services.sources.GitlabSource
import cats.effect.IOApp
import cats.effect.IO

object Main extends IOApp.Simple:
  import utils._
  import domain.registry._

  val exportDestination = "./export.xlsx"
  val registrySource = "./registry.json"

  def run: IO[Unit] =
    val content = Source.fromFile(registrySource).getLines.mkString("\n")
    val registry = json.parse[Registry](content)

    def prepareForSource(
        project: domain.project.Project
    ): Option[Project] =
      registry.projects.find(_.id == project.id)

    val gitlabApi = GitlabApi.make[IO](registry.host, registry.token.some)
    val service =
      DependencyService.make[IO, Project](
        source = GitlabSource.make(gitlabApi),
        prepareForSource = prepareForSource,
        reporter = PythonDependencyReporter.forIo,
        exporter = ExcelExporter.make(
          ExcelExporter.dependencies.toSheet,
          exportDestination
        )
      )

    service.checkDependencies(registry.projects.map {
      case Project(id, name, sources, branch) =>
        domain.project.Project(id, name)
    })
