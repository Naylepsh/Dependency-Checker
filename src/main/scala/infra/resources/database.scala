package infra.resources

import cats.*
import cats.effect.*
import cats.implicits.*
import ciris.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*

object database:
  case class Config(
      path: String,
      username: String,
      password: String
  )
  object Config:
    def load[F[_]: Async] =
      databaseConfig.load[F]

  private val databaseConfig =
    (
      env("DATABASE_PATH"),
      env("DATABASE_USER"),
      env("DATABASE_PASSWORD")
    ).parMapN {
      (path, user, password) => Config(path, user, password)
    }

  def makeTransactorResource[F[_]: Async](
      config: Config
  ): Resource[F, HikariTransactor[F]] =
    for
      ce <- ExecutionContexts.fixedThreadPool[F](32)
      xa <- HikariTransactor.newHikariTransactor[F](
        "org.sqlite.JDBC",
        s"jdbc:${config.path}",
        config.username,
        config.password,
        ce
      )
    yield xa

  def checkSQLiteConnection[F[_]: MonadCancelThrow](
      xa: Transactor[F]
  ): F[Unit] =
    sql"SELECT 1".query[Int].unique.transact(xa).void
