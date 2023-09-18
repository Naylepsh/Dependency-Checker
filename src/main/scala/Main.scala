import cats.effect.{ ExitCode, IO, IOApp }
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import cats.syntax.all.*
import org.legogroup.woof.{ *, given }
import persistence.ScanResultRepository
import persistence.DependencyRepository
import persistence.ProjectScanConfigRepository
import scanning.application.ProjectScanConfigService
import scanning.application.ProjectSummaryService
import scanning.application.ProjectController
import scanning.application.StaticFileController
import scanning.application.RootController
import processor.TaskProcessor
import scanning.application.ScanningController
import scanning.application.LoggingMiddleware
import concurrent.duration.*
import gitlab.GitlabApi
import scanning.infra.sources.GitlabSource
import scanning.application.DependencyScanner
import scanning.infra.packageindexes.Pypi
import scanning.application.services.ScanningService
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import config.AppConfig
import advisory.GithubAdvisory
import advisory.Advisory

object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] = runServer

  private def resources(config: AppConfig)(using Logger[IO]) = (
    persistence.database
      .makeSqliteTransactorResource[IO](config.database)
      .evalTap(persistence.database.checkSQLiteConnection),
    HttpClientCatsBackend.resource[IO](),
    TaskProcessor.make[IO](
      config.workerCount,
      false.pure,
      10.seconds.some
    )
  ).tupled

  private val ioLogger: IO[DefaultLogger[IO]] =
    given Filter  = Filter.everything
    given Printer = ColorPrinter()
    DefaultLogger.makeIo(Output.fromConsole)

  def runServer =
    ioLogger.flatMap: logger =>
      given Logger[IO] = logger
      AppConfig.load[IO].flatMap: config =>
        resources(config).use: (xa, backend, processor) =>
          val gitlabApi = GitlabApi.make[IO](
            backend,
            config.gitlab.host,
            config.gitlab.token
          )
          val source   = GitlabSource.make(gitlabApi)
          val scanner  = DependencyScanner.make(Pypi(backend))
          val advisory = Advisory.make(GithubAdvisory.make[IO])

          val scanResultRepository = ScanResultRepository.make(
            xa,
            DependencyRepository.make(xa)
          )
          val projectRepository = ProjectScanConfigRepository.make(xa)

          val scanResultService = ScanResultRepository.make(
            xa,
            DependencyRepository.make(xa)
          )
          val projectService =
            ProjectScanConfigService.make(projectRepository)
          val summaryService =
            ProjectSummaryService.make(scanResultRepository)
          val scanningService =
            ScanningService.make[IO](
              source,
              scanner,
              scanResultRepository,
              advisory
            )

          val projectController =
            ProjectController.make(projectService, summaryService)
          val staticFileController = StaticFileController.make[IO]
          val rootController       = RootController.make[IO]
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

          HttpServer[IO]
            .newEmber(config.server, routes.orNotFound)
            .useForever
            .as(ExitCode.Success)
