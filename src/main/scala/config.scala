import cats.effect.kernel.Async
import cats.implicits.*
import ciris.*
import com.comcast.ip4s.*
import persistence.database.Config as DatabaseConfig

object config:
  case class GitlabConfig(
      token: Option[String],
      host: String
  )

  case class ServerConfig(host: Host, port: Port)

  case class AppConfig(
      database: DatabaseConfig,
      gitlab: GitlabConfig,
      server: ServerConfig,
      workerCount: Int
  )
  object AppConfig:
    def load[F[_]: Async] =
      (persistence.database.config, gitlabConfig, tasksConfig)
        .parTupled
        .map(AppConfig.apply(_, _, serverConfig, _))
        .load[F]

  private val gitlabConfig = (
    env("GITLAB_TOKEN").option,
    env("GITLAB_HOST").option.map(_.getOrElse("gitlab.com")),
  ).parMapN(GitlabConfig.apply)

  private val tasksConfig =
    env("WORKER_COUNT").as[Int].option.map(_.getOrElse(1))

  private val serverConfig = ServerConfig(
    ipv4"0.0.0.0",
    port"8080"
  )
