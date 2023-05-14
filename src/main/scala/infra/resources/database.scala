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

  def makeTransactorResource[F[_]: Async](
      config: Config
  ): Resource[F, Transactor[F]] =
    for
      ce <- ExecutionContexts.fixedThreadPool[F](32)
      xa <- HikariTransactor.newHikariTransactor[F](
        "org.sqlite.JDBC",
        s"jdbc:${config.path}",
        config.username,
        config.password,
        ce
      )
      sqliteXa = Transactor.before.modify(
        xa,
        sql"PRAGMA foreign_keys=ON".update.run *> _
      )
    yield sqliteXa

  def checkSQLiteConnection[F[_]: MonadCancelThrow](
      xa: Transactor[F]
  ): F[Unit] =
    sql"SELECT 1".query[Int].unique.transact(xa).void
