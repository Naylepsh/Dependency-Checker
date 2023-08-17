package scanning.application

import com.comcast.ip4s.*
import cats.data.NonEmptyList
import cats.effect.std.Console
import cats.effect.{ ExitCode, IO }
import cats.syntax.all.*
import com.monovore.decline.Opts
import core.application.cli.*
import core.domain.project.{ Project, ScanReport }
import core.domain.registry.Registry
import gitlab.GitlabApi
import scanning.infra.exporters.{
  ScanDeltaExcelExporter,
  ScanReportExcelExporter
}
import scanning.infra.packageindexes.Pypi
import core.infra.persistance.{
  DependencyRepository,
  RegistryRepository,
  ScanResultRepository
}
import scanning.infra.sources.GitlabSource
import org.joda.time.DateTime
import org.legogroup.woof.Logger
import scanning.application.services.*
import org.http4s.ember.server.EmberServerBuilder
import concurrent.duration.*
import processor.TaskProcessor
import persistance.ProjectScanConfigRepository

object ScanningCli:
  private def makeScanningService(context: Context)(using
  Logger[IO]): ScanningService[IO] =
    val gitlabApi = GitlabApi.make[IO](
      context.backend,
      context.config.gitlab.host,
      context.config.gitlab.token
    )
    val source  = GitlabSource.make(gitlabApi)
    val scanner = DependencyScanner.make(Pypi(context.backend))
    val repository =
      ScanResultRepository.make(
        context.xa,
        DependencyRepository.make(context.xa)
      )

    ScanningService.make[IO](source, scanner, repository)

  case class ListLatestScans(limit: Int) extends Command[IO]:
    val registry = Registry.empty

    def run(): IO[ExitCode] =
      withContext: context =>
        val service = makeScanningService(context)

        for
          timestamps <- service.getLatestScansTimestamps(limit)
          _ <- Console[IO].println(
            s"Latest ${timestamps.length} scan timestamps:"
          )
          _ <- timestamps.traverse(timestamp =>
            Console[IO].println(s"- $timestamp")
          )
        yield ExitCode.Success

  case class DeleteScans(timestamps: NonEmptyList[DateTime])
      extends Command[IO]:
    val registry = Registry.empty

    def run(): IO[ExitCode] =
      withContext: context =>
        val service = makeScanningService(context)

        service.deleteScans(timestamps).as(ExitCode.Success)

  case class ExportScanDelta(
      exportPath: String,
      registryPath: String,
      leftTimestamp: DateTime,
      rightTimestamp: DateTime
  ) extends Command[IO]:
    def run(): IO[ExitCode] =
      withContext: context =>
        val dependencyRepository = DependencyRepository.make(context.xa)
        val repository =
          ScanResultRepository.make(context.xa, dependencyRepository)
        val exporter = ScanDeltaExcelExporter.make[IO](exportPath)
        val service  = ScanDeltaExportService.make(exporter, repository)

        RegistryRepository.fileBased(registryPath).get().flatMap {
          case Left(_) => ExitCode.Error.pure
          case Right(registry) =>
            service.exportDeltas(
              registry.projects.map: project =>
                Project(project.id, project.name),
              leftTimestamp,
              rightTimestamp
            ).as(ExitCode.Success)
        }

  case class ExportScanReports(exportPath: String, registryPath: String)
      extends Command[IO]:
    def run(): IO[ExitCode] =
      withContext: context =>
        val registryRepository =
          RegistryRepository.fileBased(registryPath)
        val exporter = ScanReportExcelExporter.make[IO](exportPath)
        val repository = ScanResultRepository.make(
          context.xa,
          DependencyRepository.make(context.xa)
        )
        val service = ScanReportExportService.make(exporter, repository)

        registryRepository.get().flatMap {
          case Left(_) => IO.unit
          case Right(registry) =>
            service.exportScanResults(registry.projects.map(project =>
              Project(project.id, project.name)
            ))
        }.as(ExitCode.Success)

  case class MigrateRegistry(registryPath: String) extends Command[IO]:
    def run(): IO[ExitCode] =
      withContext: context =>
        val registryRepository =
          RegistryRepository.fileBased(registryPath)
        val scanConfigRepository = ProjectScanConfigRepository.make(context.xa)

        registryRepository.get().flatMap:
          case Right(registry) =>
            val configs = registry.projects.map: config =>
              val sources = config.sources.map:
                case core.domain.registry.DependencySource.TxtSource(path) =>
                  core.domain.dependency.DependencySource.TxtSource(path)
                case core.domain.registry.DependencySource.TomlSource(
                      path,
                      group
                    ) => core.domain.dependency.DependencySource.TomlSource(
                    path,
                    group
                  )
              core.domain.project.ProjectScanConfig(
                core.domain.project.Project(config.id, config.name),
                sources,
                config.enabled,
                config.branch
              )
            configs.traverse(scanConfigRepository.save).as(ExitCode.Success)

  object WebServer extends Command[IO]:
    def run(): IO[ExitCode] =
      withContext: context =>
        val scanResultService = ScanResultRepository.make(
          context.xa,
          DependencyRepository.make(context.xa)
        )
        val scanResultRepository = ScanResultRepository.make(
          context.xa,
          DependencyRepository.make(context.xa)
        )
        val projectRepository = ProjectScanConfigRepository.make(context.xa)
        val projectService    = ProjectService.make(projectRepository)
        val projectController = ProjectController.make(projectService)

        val staticFileController = StaticFileController.make[IO]

        TaskProcessor.make[IO](1, false.pure, 10.seconds.some).use: processor =>
          val scanningService = makeScanningService(context)
          val scanReportController =
            ScanningController.make(
              scanningService,
              projectRepository,
              processor
            )

          val routes = scanReportController.routes 
            <+> projectController.routes 
            <+> staticFileController.routes

          EmberServerBuilder
            .default[IO]
            .withHost(ipv4"0.0.0.0")
            .withPort(port"8080")
            .withHttpApp(routes.orNotFound)
            .build
            .useForever
            .as(ExitCode.Success)

  val exportLocationOpt =
    Opts.option[String]("export-path", "Path to save the export to")
  val registryLocationOpt =
    Opts.option[String]("registry-path", "Path to JSON registry")
  val limitOpt =
    Opts.option[Int]("scan-limit", "Numbers of latest scans to consider")
  val scanTimestampsOpts: Opts[NonEmptyList[DateTime]] = Opts.option[String](
    "timestamps",
    "Comma-separated list of timestamps"
  ).mapValidated: input =>
    input
      .split(",")
      .toList
      .traverse(validateTimestamp)
      .andThen: timestamps =>
        NonEmptyList
          .fromList(timestamps)
          .toValidNel("Empty sequence is not valid")

  val listScansOpts = Opts.subcommand(
    name = "list-scans",
    help = "List the timestamp of the latest scans"
  )(limitOpt.map(ListLatestScans.apply))

  val deleteScansOpts = Opts.subcommand(
    name = "delete-scans",
    help = "Delete the scans by their timestamps"
  )(scanTimestampsOpts.map(DeleteScans.apply))

  val exportScanOpts = Opts.subcommand(
    name = "export-scan",
    help = "Export the results of the latest scan to an excel file"
  )((exportLocationOpt, registryLocationOpt).mapN(ExportScanReports.apply))

  val exportDeltaOpts = Opts.subcommand(
    name = "export-delta",
    help = "Export the difference between two scans to an excel file"
  )((
    exportLocationOpt,
    registryLocationOpt,
    timestampOpt("left-timestamp"),
    timestampOpt("right-timestamp")
  ).mapN(ExportScanDelta.apply))

  val webServerOpts = Opts.subcommand(
    name = "web",
    help = "Expose web server"
  )(Opts.unit.as(WebServer))

  val migrateRegistryOpts = Opts.subcommand(
    name = "migrate-registry",
    help = "Migrate registry from json to sql database"
  )(registryLocationOpt.map(MigrateRegistry.apply))

  val allOpts = listScansOpts
    .orElse(deleteScansOpts)
    .orElse(exportScanOpts)
    .orElse(exportDeltaOpts)
    .orElse(webServerOpts)
    .orElse(migrateRegistryOpts)
