package application

import application.config.AppConfig
import application.services.{
  ExportingService,
  PythonDependencyReporter,
  ScanningService
}
import cats.data.NonEmptyList
import cats.effect.implicits.*
import cats.effect.std.Console
import cats.effect.{ ExitCode, IO }
import cats.implicits.*
import com.monovore.decline.*
import domain.project.{ Project, ScanReport }
import domain.registry.Registry
import doobie.util.transactor.Transactor
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
import org.joda.time.DateTime
import org.legogroup.woof.{ *, given }
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend
import sttp.client3.httpclient.cats.HttpClientCatsBackend

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

  private class Context(
      val config: AppConfig,
      val xa: Transactor[IO],
      val backend: SttpBackend[IO, WebSockets]
  )

  private def makeScanningService(
      context: Context,
      registry: Registry,
      parallelGroupSize: Int
  )(using Logger[IO]): ScanningService[IO] =
    val gitlabApi = GitlabApi.make[IO](
      context.backend,
      registry.host,
      context.config.gitlabToken
    )
    val source = GitlabSource.make(gitlabApi)
    val prepareProjectForSource = (project: domain.project.Project) =>
      registry.projects.find(_.id == project.id)
    val reporter =
      PythonDependencyReporter.make(Pypi(context.backend), parallelGroupSize)
    val repository =
      ScanResultRepository.make(
        context.xa,
        DependencyRepository.make(context.xa)
      )

    ScanningService.make[IO, domain.registry.Project](
      source,
      prepareProjectForSource,
      reporter,
      repository
    )

  private val parallelGroupSize = 10

  private def withContext(f: Context => Logger[IO] ?=> IO[ExitCode])
      : IO[ExitCode] =
    AppConfig.load[IO].flatMap { config =>
      resources(config).use {
        case (xa, backend) =>
          logging
            .forConsoleIo()
            .flatMap(logger => f(Context(config, xa, backend))(using logger))
      }
    }

  case class ScanRepositories(registryPath: String) extends Command[IO]:
    def run(): IO[ExitCode] =
      withContext { context =>
        val registryRepository =
          RegistryRepository.fileBased(registryPath)

        registryRepository.get().flatMap {
          case Left(_) => IO.unit
          case Right(registry) =>
            val service = makeScanningService(
              context,
              registry,
              parallelGroupSize
            )

            service.scan(registry.projects.collect {
              case domain.registry.Project(id, name, _, true, _) =>
                domain.project.Project(id, name)
            })
        }.as(ExitCode.Success)
      }

  case class ListLatestScans(limit: Int) extends Command[IO]:
    val registry = Registry.empty

    def run(): IO[ExitCode] =
      withContext { context =>
        val service = makeScanningService(
          context,
          registry,
          parallelGroupSize
        )

        for
          timestamps <- service.getLatestScansTimestamps(limit)
          _ <- Console[IO].println(
            s"Latest ${timestamps.length} scan timestamps:"
          )
          _ <- timestamps.traverse(timestamp =>
            Console[IO].println(s"- $timestamp")
          )
        yield ExitCode.Success
      }

  case class DeleteScans(timestamps: NonEmptyList[DateTime])
      extends Command[IO]:
    val registry = Registry.empty

    def run(): IO[ExitCode] =
      withContext { context =>
        val service = makeScanningService(
          context,
          registry,
          parallelGroupSize
        )

        service.deleteScans(timestamps).as(ExitCode.Success)
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
  val scanTimestampsOpts: Opts[NonEmptyList[DateTime]] = Opts.option[String](
    "timestamps",
    "Comma-separated list of timestamps"
  ).mapValidated(input =>
    input
      .split(",")
      .toList
      .traverse(str =>
        Either
          .catchNonFatal(DateTime.parse(str))
          .leftMap(_.toString).toValidatedNel
      )
      .andThen(timestamps =>
        NonEmptyList
          .fromList(timestamps)
          .toValidNel("Empty sequence is not valid")
      )
  )

  val scanOpts = Opts.subcommand(
    name = "scan",
    help = "Scan the projects' dependencies"
  )(registryLocationOpt.map(ScanRepositories.apply))

  val listScansOpts = Opts.subcommand(
    name = "list-scans",
    help = "List the timestamp of the latest scans"
  )(limitOpt.map(ListLatestScans.apply))

  val deleteScansOpts = Opts.subcommand(
    name = "delete-scans",
    help = "Delete the scans by their timestamps"
  )(scanTimestampsOpts.map(DeleteScans.apply))

  val exportOpts = Opts.subcommand(
    name = "export",
    help = "Export the results of the latest scan to an excel file"
  )((exportLocationOpt, registryLocationOpt).mapN(ExportScanReports.apply))