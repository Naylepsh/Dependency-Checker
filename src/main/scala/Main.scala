import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent._
import cats._
import cats.implicits._
import cats.effect._
import org.legogroup.woof.{given, *}
import infra.logging
import infra.GitlabApi
import infra.persistance.RegistryRepository
import infra.sources.GitlabSource
import infra.exporters.ExcelExporter
import application.DependencyService
import application.PythonDependencyReporter

object Main extends IOApp.Simple:
  import domain.registry._

  val exportDestination = "./export.xlsx"
  val registrySource = "./registry.json"

  def run: IO[Unit] =
    val registryRepository = RegistryRepository.fileBased(registrySource)

    for
      given Logger[IO] <- logging.forConsoleIo()
      registry <- registryRepository.get()
      prepareForSource = (project: domain.project.Project) =>
        registry.projects.find(_.id == project.id)
      gitlabApi = GitlabApi.make[IO](registry.host, registry.token.some)
      service =
        DependencyService.make[IO, Project](
          source = GitlabSource.make(gitlabApi),
          prepareForSource = prepareForSource,
          reporter = PythonDependencyReporter.forIo,
          exporter = ExcelExporter.make(
            ExcelExporter.dependencies.toSheet,
            exportDestination
          )
        )
      _ <- service.checkDependencies(registry.projects.collect {
        case Project(id, name, sources, true, branch) =>
          domain.project.Project(id, name)
      })
    yield ()
