import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.Duration
import scala.io.Source

import application.PythonDependencyReporter
import application.ScanningService
import cats.*
import cats.effect.*
import cats.implicits.*
import ciris.*
import infra.exporters.ExcelExporter
import infra.packageindexes.Pypi
import infra.persistance.{ DependencyRepository, RegistryRepository }
import infra.resources.database
import infra.sources.GitlabSource
import infra.{ GitlabApi, logging }
import org.legogroup.woof.{ *, given }
import sttp.client3.httpclient.cats.HttpClientCatsBackend

object Main extends IOApp.Simple:
  import domain.registry.*

  val exportDestination = "./export.xlsx"
  val registrySource    = "./registry.json"

  def run: IO[Unit] =
    val registryRepository = RegistryRepository.fileBased(registrySource)

    database.Config.load[IO].flatMap { config =>
      (
        database.makeTransactorResource[IO](config).evalTap(
          database.checkSQLiteConnection
        ),
        HttpClientCatsBackend.resource[IO]()
      ).tupled.use {
        case (xa, backend) =>
          println(config)
          for
            given Logger[IO] <- logging.forConsoleIo()
            _ <- registryRepository.get().flatMap(_.fold(
              _ => IO.unit,
              registry =>
                val prepareForSource = (project: domain.project.Project) =>
                  registry.projects.find(_.id == project.id)
                val gitlabApi =
                  GitlabApi.make[IO](
                    backend,
                    registry.host,
                    registry.token.some
                  )
                val service =
                  ScanningService.make[IO, Project](
                    source = GitlabSource.make(gitlabApi),
                    prepareForSource = prepareForSource,
                    reporter = PythonDependencyReporter.forIo(Pypi(backend)),
                    exporter = ExcelExporter.make(
                      ExcelExporter.dependencies.toSheet,
                      exportDestination
                    ),
                    repository = DependencyRepository.make(xa)
                  )
                service.scan(registry.projects.collect {
                  case Project(id, name, sources, true, branch) =>
                    domain.project.Project(id, name)
                })
            ))
          yield ()
      }
    }
