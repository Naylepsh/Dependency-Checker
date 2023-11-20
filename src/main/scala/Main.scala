import advisory.{ Advisory, GithubAdvisory }
import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.all.*
import com.comcast.ip4s.*
import config.AppConfig
import controllers.{ LoggingMiddleware, RootController, StaticFileController }
import gitlab.GitlabApi
import jira.{ Jira, Template, TicketTemplate }
import org.http4s.ember.server.EmberServerBuilder
import org.legogroup.woof.{ *, given }
import persistence.{
  DependencyRepository,
  ProjectScanConfigRepository,
  ScanResultRepository
}
import processor.TaskProcessor
import scanning.application.*
import scanning.application.services.ScanningService
import scanning.infra.packageindexes.Pypi
import scanning.infra.sources.GitlabSource
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import update.controllers.UpdateController
import update.repositories.UpdateRepository
import update.services.*

import concurrent.duration.*

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
          // Setup core layer
          val gitlabApi = GitlabApi.make[IO](
            backend,
            config.gitlab.host,
            config.gitlab.token
          )
          val source   = GitlabSource.make(gitlabApi)
          val scanner  = DependencyScanner.make(Pypi(backend))
          val advisory = Advisory.make(GithubAdvisory.make[IO])

          // Setup data access layer
          val scanResultRepository = ScanResultRepository.make(
            xa,
            DependencyRepository.make(xa)
          )
          val projectRepository = ProjectScanConfigRepository.make(xa)
          val updateRepository  = UpdateRepository.make(xa)

          // Setup logic layer
          val scanResultService = ScanResultRepository.make(
            xa,
            DependencyRepository.make(xa)
          )
          val projectService =
            ProjectScanConfigService.make(projectRepository)
          val summaryService =
            ProjectSummaryService.make(scanResultRepository)
          val jiraNotificationService = (config.jira, config.autoUpdateJira)
            .tupled
            .flatMap: (jiraConfig, autoUpdateConfig) =>
              TicketTemplate.fromFiles(
                "./templates/jira/summary.txt",
                "./templates/jira/description.json"
              ).toOption.map: template =>
                val jira = Jira.make[IO](jiraConfig, backend, template)
                JiraNotificationService.make(
                  jira,
                  autoUpdateConfig.projectKey,
                  autoUpdateConfig.issueType
                )
            .getOrElse(JiraNotificationService.noop[IO])
          val updateService =
            UpdateService.make(
              updateRepository,
              projectRepository,
              gitlabApi,
              jiraNotificationService
            )
          val updateGateway =
            UpdateGateway.make[IO](updateRepository, updateService, processor)
          val scanningService =
            ScanningService.make[IO](
              source,
              scanner,
              scanResultRepository,
              projectRepository,
              advisory,
              processor,
              updateGateway
            )

          // Setup presentation layer
          val projectController =
            ProjectController.make(projectService, summaryService)
          val staticFileController = StaticFileController.make[IO]
          val rootController       = RootController.make[IO]
          val scanReportController =
            ScanningController.make(scanningService, projectRepository)
          val updateController = UpdateController.make(updateService)

          val routes =
            LoggingMiddleware.wrap:
              rootController.routes
                <+> staticFileController.routes
                <+> scanReportController.routes
                <+> projectController.routes
                <+> updateController.routes

          HttpServer[IO]
            .newEmber(config.server, routes.orNotFound)
            .useForever
            .as(ExitCode.Success)
