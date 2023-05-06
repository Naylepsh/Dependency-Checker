package application

import application.services.{
  ExportingService,
  PythonDependencyReporter,
  ScanningService
}
import cats.effect.{ ExitCode, IO }
import cats.implicits.*
import com.monovore.decline.*
import domain.project.{ Project, ScanReport }
import infra.exporters.ExcelExporter
import infra.packageindexes.Pypi
import infra.persistance.{
  DependencyRepository,
  RegistryRepository,
  ScanResultRepository
}
import infra.resources.database
import infra.sources.GitlabSource
import infra.{ GitlabApi, logging }
import org.legogroup.woof.{ *, given }
import sttp.client3.httpclient.cats.HttpClientCatsBackend

object cli:
  case class ScanRepositories(registryPath: String)
  object ScanRepositories:
    def run(command: ScanRepositories): IO[ExitCode] =
      val registryRepository =
        RegistryRepository.fileBased(command.registryPath)

      database.Config.load[IO].flatMap { config =>
        (
          database.makeTransactorResource[IO](config).evalTap(
            database.checkSQLiteConnection
          ),
          HttpClientCatsBackend.resource[IO]()
        ).tupled.use {
          case (xa, backend) =>
            for
              given Logger[IO] <- logging.forConsoleIo()
              _ <- registryRepository.get().flatMap {
                case Left(_) => IO.unit
                case Right(registry) =>
                  val gitlabApi =
                    GitlabApi.make[IO](
                      backend,
                      registry.host,
                      registry.token.some
                    )
                  val service =
                    ScanningService.make[IO, domain.registry.Project](
                      source = GitlabSource.make(gitlabApi),
                      prepareForSource = (project: domain.project.Project) =>
                        registry.projects.find(_.id == project.id),
                      reporter = PythonDependencyReporter.forIo(Pypi(backend)),
                      repository = ScanResultRepository.make(
                        xa,
                        DependencyRepository.make(xa)
                      )
                    )

                  service.scan(registry.projects.collect {
                    case domain.registry.Project(id, name, _, true, _) =>
                      domain.project.Project(id, name)
                  })
              }
            yield ExitCode.Success
        }
      }

  case class ExportScanReports(exportPath: String, registryPath: String)
  object ExportScanReports:
    def run(command: ExportScanReports): IO[ExitCode] =
      val registryRepository =
        RegistryRepository.fileBased(command.registryPath)

      database.Config.load[IO].flatMap { config =>
        database.makeTransactorResource[IO](config).evalTap(
          database.checkSQLiteConnection
        ).use(xa =>
          for
            given Logger[IO] <- logging.forConsoleIo()
            _ <- registryRepository.get().flatMap {
              case Left(_) => IO.unit
              case Right(registry) =>
                val exporter = ExcelExporter.make[IO, ScanReport](
                  ExcelExporter.dependencies.toSheet,
                  command.exportPath
                )
                val repository = ScanResultRepository.make(
                  xa,
                  DependencyRepository.make(xa)
                )
                val service = ExportingService.make(exporter, repository)

                service.exportScanResults(registry.projects.map(project =>
                  Project(project.id, project.name)
                ))
            }
          yield ExitCode.Success
        )
      }

  val exportLocationOpt =
    Opts.option[String]("export-path", "Path to save the export to")
  val registryLocationOpt =
    Opts.option[String]("registry-path", "Path to JSON registry")

  val scanOpts = Opts.subcommand(
    name = "scan",
    help = "Scan the projects' dependencies"
  )(registryLocationOpt.map(ScanRepositories.apply))

  val exportOpts = Opts.subcommand(
    name = "export",
    help = "Export the results of the latest scan to an excel file"
  )((exportLocationOpt, registryLocationOpt).mapN(ExportScanReports.apply))
