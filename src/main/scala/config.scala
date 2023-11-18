import cats.effect.kernel.Async
import cats.implicits.*
import ciris.*
import com.comcast.ip4s.*
import core.{ Newtype, Wrapper }
import jira.{Config => JiraConfig, _}
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
      workerCount: Int,
      jira: Option[JiraConfig]
  )
  object AppConfig:
    def load[F[_]: Async] =
      (persistence.database.config, gitlabConfig, tasksConfig, jiraConfig)
        .parTupled
        .map(AppConfig.apply(_, _, serverConfig, _, _))
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

  // A bit of boilerplate for being able to use ciris with opaque types seamlessly
  extension [F[_], A](cv: ConfigValue[F, A])
    def fallback[Raw](value: Raw)(using
    ev: Wrapper[Raw, A]): ConfigValue[F, A] =
      cv.default(ev.iso.get(value))
  given [A, B](
      using wp: Wrapper[A, B],
      cd: ConfigDecoder[String, A]
  ): ConfigDecoder[String, B] =
    cd.map(a => wp.iso.get(a))

  private val jiraConfig = (
    env("JIRA_USERNAME").as[Username].option,
    env("JIRA_PASSWORD").as[Password].option,
    env("JIRA_ADDRESS").as[Address].option
  ).parMapN((_, _, _).tupled.map(JiraConfig.apply))
