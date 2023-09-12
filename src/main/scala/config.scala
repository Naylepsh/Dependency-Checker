import cats.effect.kernel.Async
import cats.implicits.*
import ciris.*
import persistence.database.Config as DatabaseConfig

object config:
  case class GitlabConfig(
      token: Option[String],
      host: String
  )

  case class AppConfig(
      database: DatabaseConfig,
      gitlab: GitlabConfig,
      workerCount: Int
  )
  object AppConfig:
    def load[F[_]: Async] =
      (persistence.database.config, gitlabConfig, tasksConfig)
        .parTupled
        .map(AppConfig.apply)
        .load[F]

  private val gitlabConfig = (
    env("GITLAB_TOKEN").option,
    env("GITLAB_HOST").option.map(_.getOrElse("gitlab.com")),
  ).parMapN(GitlabConfig.apply)

  private val tasksConfig =
    env("WORKER_COUNT").as[Int].option.map(_.getOrElse(1))
