package upkeep.application

import core.application.cli.Command
import cats.effect.ExitCode
import cats.effect.IO
import cats.implicits.*
import core.application.cli.{ Context, registryLocationOpt, withContext }
import core.infra.persistance.RegistryRepository
import core.infra.GitlabApi
import upkeep.infra.ProjectRepository
import upkeep.infra.UpkeepRepository
import org.legogroup.woof.Logger
import com.monovore.decline.*

object UpkeepCli:
  case class UpkeepSingle(registryPath: String, dependencyName: String)
      extends Command[IO]:
    def run(): IO[ExitCode] =
      withContext: context =>
        val registryRepository = RegistryRepository.fileBased(registryPath)

        registryRepository.get().flatMap {
          case Left(_) => ExitCode.Error.pure
          case Right(registry) =>
            makeUpkeepService(registryRepository, registry, context)
              .updateAffectedProjects(dependencyName)
              .as(ExitCode.Success)
        }

  private val dependencyNameOpt =
    Opts.option[String](
      "name",
      "Name of the dependency"
    )

  private val upkeepDependencyOpts = Opts.subcommand(
    name = "upkeep-single",
    help =
      "Update a single dependency to the newest known version in all affected projects"
  )((
    dependencyNameOpt,
    registryLocationOpt,
  ).mapN(UpkeepSingle.apply))

  private def makeUpkeepService(
      registryRepository: core.domain.registry.RegistryRepository[IO],
      registry: core.domain.registry.Registry,
      context: Context
  )(using Logger[IO]) =
    val gitlabApi = GitlabApi.make(
      context.backend,
      registry.host,
      context.config.gitlabToken
    )
    val upkeepRepository = UpkeepRepository.make(context.xa)
    val projectRepository =
      ProjectRepository.make(context.xa, registryRepository)
    UpkeepService.makeForGitlab(
      gitlabApi,
      projectRepository,
      upkeepRepository
    )

  val allOpts = upkeepDependencyOpts
