import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.Duration
import scala.io.Source

import application.{ DependencyService, PythonDependencyReporter }
import cats.*
import cats.effect.*
import cats.implicits.*
import infra.exporters.ExcelExporter
import infra.persistance.RegistryRepository
import infra.sources.GitlabSource
import infra.{ GitlabApi, logging }
import org.legogroup.woof.{ *, given }
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import infra.packageindexes.Pypi

object Main extends IOApp.Simple:
  import domain.registry.*

  val exportDestination = "./export.xlsx"
  val registrySource    = "./registry.json"

  def run: IO[Unit] =
    val registryRepository = RegistryRepository.fileBased(registrySource)

    HttpClientCatsBackend.resource[IO]().use { backend =>
      val pypi = Pypi(backend)

      for
        given Logger[IO] <- logging.forConsoleIo()
        registry         <- registryRepository.get()
        prepareForSource = (project: domain.project.Project) =>
          registry.projects.find(_.id == project.id)
        gitlabApi = GitlabApi.make[IO](registry.host, registry.token.some)
        service =
          DependencyService.make[IO, Project](
            source = GitlabSource.make(gitlabApi),
            prepareForSource = prepareForSource,
            reporter = PythonDependencyReporter.forIo(pypi),
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
    }
