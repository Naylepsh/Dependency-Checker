package upkeep.application

import cats.effect.{ ExitCode, IO }
import cats.implicits.*
import com.monovore.decline.*
import core.application.cli as CoreCli
import core.application.cli.*
import core.infra.GitlabApi
import core.infra.persistance.RegistryRepository
import org.legogroup.woof.Logger
import upkeep.infra.{ ProjectRepository, UpkeepRepository }

object UpkeepCli:
  case class UpkeepSingle(registryPath: String, dependencyName: String)
      extends CoreCli.Command[IO]:
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
    registryLocationOpt,
    dependencyNameOpt,
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
