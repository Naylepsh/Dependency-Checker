package application

import application.services.{
  ExportingService,
  PythonDependencyReporter,
  ScanningService
}
import cats.effect.{ ExitCode, IO }
import cats.effect.std.Console
import cats.implicits.*
import cats.effect.implicits.*
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
import application.config.AppConfig
import infra.sources.GitlabSource
import infra.{ GitlabApi, logging }
import org.legogroup.woof.{ *, given }
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import domain.registry.Registry
import sttp.client3.SttpBackend
import sttp.capabilities.WebSockets
import doobie.util.transactor.Transactor

object cli:
  trait Command[F[_]]:
    def run(): F[ExitCode]

  private def resources(config: AppConfig) =
    (
      database.makeTransactorResource[IO](config.databaseConfig).evalTap(
        database.checkSQLiteConnection
      ),
      HttpClientCatsBackend.resource[IO]()
    ).tupled

  private def makeScanningService(
      registry: Registry,
      gitlabToken: Option[String],
      parallelGroupSize: Int,
      backend: SttpBackend[IO, WebSockets],
      xa: Transactor[IO]
  )(using Logger[IO]): ScanningService[IO] =
    val gitlabApi = GitlabApi.make[IO](backend, registry.host, gitlabToken)
    val source    = GitlabSource.make(gitlabApi)
    val prepareProjectForSource = (project: domain.project.Project) =>
      registry.projects.find(_.id == project.id)
    val reporter =
      PythonDependencyReporter.make(Pypi(backend), parallelGroupSize)
    val repository =
      ScanResultRepository.make(xa, DependencyRepository.make(xa))

    ScanningService.make[IO, domain.registry.Project](
      source,
      prepareProjectForSource,
      reporter,
      repository
    )

  private val parallelGroupSize = 10

  case class ScanRepositories(registryPath: String) extends Command[IO]:
    def run(): IO[ExitCode] =
      val registryRepository =
        RegistryRepository.fileBased(registryPath)

      AppConfig.load[IO].flatMap { config =>
        resources(config).use {
          case (xa, backend) =>
            for
              given Logger[IO] <- logging.forConsoleIo()
              _ <- registryRepository.get().flatMap {
                case Left(_) => IO.unit
                case Right(registry) =>
                  val service = makeScanningService(
                    registry,
                    config.gitlabToken,
                    parallelGroupSize,
                    backend,
                    xa
                  )

                  service.scan(registry.projects.collect {
                    case domain.registry.Project(id, name, _, true, _) =>
                      domain.project.Project(id, name)
                  })
              }
            yield ExitCode.Success
        }
      }

  case class ListLatestScans(limit: Int) extends Command[IO]:
    val registry = Registry.empty

    def run(): IO[ExitCode] =
      AppConfig.load[IO].flatMap { config =>
        resources(config).use {
          case (xa, backend) =>
            for
              given Logger[IO] <- logging.forConsoleIo()
              service = makeScanningService(
                registry,
                config.gitlabToken,
                parallelGroupSize,
                backend,
                xa
              )
              timestamps <- service.getLatestScansTimestamps(limit)
              _ <- Console[IO].println(
                s"Latest ${timestamps.length} scan timestamps:"
              )
              _ <- timestamps.traverse(timestamp =>
                Console[IO].println(s"- $timestamp")
              )
            yield ExitCode.Success
        }
      }

  case class ExportScanReports(exportPath: String, registryPath: String)
      extends Command[IO]:
    def run(): IO[ExitCode] =
      val registryRepository =
        RegistryRepository.fileBased(registryPath)
      val exporter = ExcelExporter.make[IO](exportPath)

      AppConfig.load[IO].flatMap { config =>
        database.makeTransactorResource[IO](config.databaseConfig).evalTap(
          database.checkSQLiteConnection
        ).use(xa =>
          for
            given Logger[IO] <- logging.forConsoleIo()
            _ <- registryRepository.get().flatMap {
              case Left(_) => IO.unit
              case Right(registry) =>
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
  val limitOpt =
    Opts.option[Int]("scan-limit", "Numbers of latest scans to consider")

  val scanOpts = Opts.subcommand(
    name = "scan",
    help = "Scan the projects' dependencies"
  )(registryLocationOpt.map(ScanRepositories.apply))

  val listScansOpts = Opts.subcommand(
    name = "list-scans",
    help = "List the timestamp of the latest scans"
  )(limitOpt.map(ListLatestScans.apply))

  val exportOpts = Opts.subcommand(
    name = "export",
    help = "Export the results of the latest scan to an excel file"
  )((exportLocationOpt, registryLocationOpt).mapN(ExportScanReports.apply))
