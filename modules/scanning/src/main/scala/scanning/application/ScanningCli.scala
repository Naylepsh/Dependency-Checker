package scanning.application

import core.application.cli.Context
import core.application.services.ScanningService
import core.domain.registry.Registry
import org.legogroup.woof.Logger
import cats.effect.IO
import cats.effect.std.Console
import cats.syntax.all.*
import core.infra.GitlabApi
import core.infra.sources.GitlabSource
import core.application.services.PythonDependencyReporter
import core.infra.packageindexes.Pypi
import core.infra.persistance.ScanResultRepository
import core.infra.persistance.DependencyRepository
import core.application.cli.{ Command, validateTimestamp, withContext }
import cats.effect.ExitCode
import core.infra.persistance.RegistryRepository
import com.monovore.decline.Opts
import cats.data.NonEmptyList
import org.joda.time.DateTime
import core.infra.exporters.ScanDeltaExcelExporter
import core.application.services.ScanDeltaExportService
import core.domain.project.{ Project, ScanReport }
import core.infra.exporters.ScanReportExcelExporter
import core.application.services.ScanReportExportService
import core.application.cli.timestampOpt

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
      PythonDependencyReporter.make(Pypi(context.backend), parallelGroupSize)
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

        registryRepository.get().flatMap {
          case Left(_) => ExitCode.Error.pure
          case Right(registry) =>
            val service = makeScanningService(context, parallelGroupSize)

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
        RegistryRepository.fileBased(registryPath).get().flatMap {
          case Left(_) => ExitCode.Error.pure
          case Right(registry) =>
            val repository = ScanResultRepository.make(
              context.xa,
              DependencyRepository.make(context.xa)
            )
            val exporter = ScanDeltaExcelExporter.make[IO](exportPath)
            val service  = ScanDeltaExportService.make(exporter, repository)

            service.exportDeltas(
              registry.projects.map(project =>
                Project(project.id, project.name)
              ),
              leftTimestamp,
              rightTimestamp
            ).as(ExitCode.Success)
        }

  case class ExportScanReports(exportPath: String, registryPath: String)
      extends Command[IO]:
    def run(): IO[ExitCode] =
      val registryRepository =
        RegistryRepository.fileBased(registryPath)
      val exporter = ScanReportExcelExporter.make[IO](exportPath)

      withContext: context =>
        registryRepository.get().flatMap {
          case Left(_) => IO.unit
          case Right(registry) =>
            val repository = ScanResultRepository.make(
              context.xa,
              DependencyRepository.make(context.xa)
            )
            val service = ScanReportExportService.make(exporter, repository)

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
