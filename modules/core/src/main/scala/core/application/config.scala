package core.application

import cats.effect.kernel.Async
import cats.implicits.*
import ciris.*
import core.infra.resources.database.Config as DatabaseConfig

object config:
  case class GitlabConfig(
      token: Option[String],
      host: String
  )
  case class AppConfig(
      database: DatabaseConfig,
      gitlab: GitlabConfig
  )
  object AppConfig:
    def load[F[_]: Async] =
      (databaseConfig, gitlabConfig).parTupled.map(AppConfig.apply).load[F]

  private val databaseConfig =
    (
      env("DATABASE_PATH"),
      env("DATABASE_USER"),
      env("DATABASE_PASSWORD")
    ).parMapN {
      (path, user, password) => DatabaseConfig(path, user, password)
    }

  private val gitlabConfig = (
    env("GITLAB_TOKEN").option,
    env("GITLAB_HOST").option.map(_.getOrElse("gitlab.com"))
  ).parMapN(GitlabConfig.apply)
