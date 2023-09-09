package upkeep.application

import cats.effect.{ ExitCode, IO }
import cats.implicits.*
import com.monovore.decline.*
import core.application.cli.*
import core.application.cli as CoreCli
import gitlab.GitlabApi
import org.legogroup.woof.Logger
import upkeep.infra.{ ProjectRepository, UpkeepRepository }

object UpkeepCli:
  case class UpkeepSingle(registryPath: String, dependencyName: String)
      extends CoreCli.Command[IO]:
    def run(): IO[ExitCode] =
      withContext: context =>
        makeUpkeepService(context)
          .updateAffectedProjects(dependencyName)
          .as(ExitCode.Success)

  private val dependencyNameOpt =
    Opts.option[String](
      "name",
      "Name of the dependency"
    )
  val registryLocationOpt =
    Opts.option[String]("registry-path", "Path to JSON registry")

  private val upkeepDependencyOpts = Opts.subcommand(
    name = "upkeep-single",
    help =
      "Update a single dependency to the newest known version in all affected projects"
  )((
    registryLocationOpt,
    dependencyNameOpt,
  ).mapN(UpkeepSingle.apply))

  private def makeUpkeepService(
      context: Context
  )(using Logger[IO]) =
    val gitlabApi = GitlabApi.make(
      context.backend,
      context.config.gitlab.host,
      context.config.gitlab.token
    )
    val upkeepRepository = UpkeepRepository.make(context.xa)
    val projectRepository =
      ProjectRepository.make(context.xa)
    UpkeepService.makeForGitlab(
      gitlabApi,
      projectRepository,
      upkeepRepository
    )

  val allOpts = upkeepDependencyOpts
