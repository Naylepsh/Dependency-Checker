package application

import cats.effect.kernel.Async
import cats.implicits.*
import ciris.*
import infra.resources.database

object config:
  case class AppConfig(
      databaseConfig: database.Config,
      gitlabToken: Option[String]
  )
  object AppConfig:
    def load[F[_]: Async] =
      (databaseConfig, gitlabToken).parTupled.map(AppConfig.apply).load[F]

  private val databaseConfig =
    (
      env("DATABASE_PATH"),
      env("DATABASE_USER"),
      env("DATABASE_PASSWORD")
    ).parMapN {
      (path, user, password) => database.Config(path, user, password)
    }

  private val gitlabToken = env("GITLAB_TOKEN").option
