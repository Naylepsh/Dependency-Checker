package scanning.application

import cats.data.NonEmptyList
import cats.effect.std.Console
import cats.effect.{ ExitCode, IO }
import cats.syntax.all.*
import com.comcast.ip4s.*
import com.monovore.decline.Opts
import core.application.cli.*
import core.domain.project.{ Project, ScanReport }
import gitlab.GitlabApi
import org.http4s.ember.server.EmberServerBuilder
import org.joda.time.DateTime
import org.legogroup.woof.Logger
import persistence.{
  DependencyRepository,
  ProjectScanConfigRepository,
  ScanResultRepository
}
import processor.TaskProcessor
import scanning.application.services.*
import scanning.infra.packageindexes.Pypi
import scanning.infra.sources.GitlabSource

import concurrent.duration.*

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
        val projectService    = ProjectScanConfigService.make(projectRepository)
        val summaryService    = ProjectSummaryService.make(scanResultRepository)
        val projectController =
          ProjectController.make(projectService, summaryService)

        val staticFileController = StaticFileController.make[IO]
        val rootController = RootController.make[IO]

        TaskProcessor.make[IO](
          context.config.workerCount,
          false.pure,
          10.seconds.some
        ).use: processor =>
          val scanningService = makeScanningService(context)
          val scanReportController =
            ScanningController.make(
              scanningService,
              projectRepository,
              processor
            )

          val routes =
            LoggingMiddleware.wrap:
                rootController.routes
                  <+> scanReportController.routes
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

  val webServerOpts = Opts.subcommand(
    name = "web",
    help = "Expose web server"
  )(Opts.unit.as(WebServer))

  val allOpts = webServerOpts
