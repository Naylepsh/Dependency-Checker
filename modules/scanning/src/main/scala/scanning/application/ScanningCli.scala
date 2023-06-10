package scanning.application

import cats.data.NonEmptyList
import cats.effect.std.Console
import cats.effect.{ ExitCode, IO }
import cats.syntax.all.*
import com.monovore.decline.Opts
import core.application.cli.*
import core.domain.project.{ Project, ScanReport }
import core.domain.registry.Registry
import core.infra.GitlabApi
import core.infra.exporters.{ ScanDeltaExcelExporter, ScanReportExcelExporter }
import core.infra.packageindexes.Pypi
import core.infra.persistance.{
  DependencyRepository,
  RegistryRepository,
  ScanResultRepository
}
import core.infra.sources.GitlabSource
import org.joda.time.DateTime
import org.legogroup.woof.Logger
import scanning.application.services._

object ScanningCli:
  private val parallelGroupSize = 10

  private def makeScanningService(
      context: Context,
      parallelGroupSize: Int
  )(using Logger[IO]): ScanningService[IO] =
    val gitlabApi = GitlabApi.make[IO](
      context.backend,
      context.config.gitlab.host,
      context.config.gitlab.token
    )
    val source = GitlabSource.make(gitlabApi)
    val reporter =
      PythonDependencyScanner.make(Pypi(context.backend), parallelGroupSize)
    val repository =
      ScanResultRepository.make(
        context.xa,
        DependencyRepository.make(context.xa)
      )

    ScanningService.make[IO](source, reporter, repository)

  case class ScanRepositories(registryPath: String) extends Command[IO]:
    def run(): IO[ExitCode] =
      withContext: context =>
        val registryRepository =
          RegistryRepository.fileBased(registryPath)
        val service = makeScanningService(context, parallelGroupSize)

        registryRepository.get().flatMap {
          case Left(_) => ExitCode.Error.pure
          case Right(registry) =>
            service
              .scan(registry.projects.filter(_.enabled))
              .as(ExitCode.Success)
        }

  case class ListLatestScans(limit: Int) extends Command[IO]:
    val registry = Registry.empty

    def run(): IO[ExitCode] =
      withContext: context =>
        val service = makeScanningService(context, parallelGroupSize)

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
        val service = makeScanningService(context, parallelGroupSize)

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

  val allOpts =
    scanOpts orElse listScansOpts orElse deleteScansOpts orElse exportScanOpts orElse exportDeltaOpts
