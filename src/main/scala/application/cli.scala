package application

import com.monovore.decline.*
import cats.implicits.*
import infra.persistance.RegistryRepository
import cats.effect.IO
import infra.resources.database
import infra.logging
import org.legogroup.woof.{ *, given }
import infra.exporters.ExcelExporter
import domain.project.ScanReport
import infra.persistance.ScanResultRepository
import infra.persistance.DependencyRepository
import application.services.ExportingService
import domain.project.Project
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import infra.GitlabApi
import application.services.ScanningService
import infra.sources.GitlabSource
import application.services.PythonDependencyReporter
import infra.packageindexes.Pypi
import cats.effect.ExitCode

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
                    ScanningService.make[IO, domain.registry.Project](
                      source = GitlabSource.make(gitlabApi),
                      prepareForSource = prepareForSource,
                      reporter = PythonDependencyReporter.forIo(Pypi(backend)),
                      repository = ScanResultRepository.make(
                        xa,
                        DependencyRepository.make(xa)
                      )
                    )

                  service.scan(registry.projects.collect {
                    case domain.registry.Project(
                          id,
                          name,
                          sources,
                          true,
                          branch
                        ) =>
                      domain.project.Project(id, name)
                  })
              ))
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
            _ <- registryRepository.get().flatMap(_.fold(
              _ => IO.unit,
              registry =>
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
            ))
          yield ExitCode.Success
        )
      }

  val exportLocationOpt   = Opts.argument[String]("export-path")
  val registryLocationOpt = Opts.argument[String]("registry-path")

  val scanOpts = Opts.subcommand(
    name = "scan",
    help = "Scan the projects' dependencies"
  )(registryLocationOpt.map(ScanRepositories.apply))

  val exportOpts = Opts.subcommand(
    name = "export",
    help = "Export the results of the latest scan to an excel file"
  )((exportLocationOpt, registryLocationOpt).mapN(ExportScanReports.apply))
