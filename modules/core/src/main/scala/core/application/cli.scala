package core.application

import cats.data.ValidatedNel 
import cats.effect.implicits.*
import cats.effect.{ ExitCode, IO }
import cats.implicits.*
import com.monovore.decline.*
import core.infra.resources.database
import core.infra.logging
import doobie.util.transactor.Transactor
import org.joda.time.DateTime
import org.legogroup.woof.{ *, given }
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend
import sttp.client3.httpclient.cats.HttpClientCatsBackend

import config.AppConfig
import services.*

object cli:
  trait Command[F[_]]:
    def run(): F[ExitCode]

  private def resources(config: AppConfig) =
    (
      database
        .makeSqliteTransactorResource[IO](config.database)
        .evalTap(database.checkSQLiteConnection),
      HttpClientCatsBackend.resource[IO]()
    ).tupled

  class Context(
      val config: AppConfig,
      val xa: Transactor[IO],
      val backend: SttpBackend[IO, WebSockets]
  )

  def withContext(f: Context => Logger[IO] ?=> IO[ExitCode]): IO[ExitCode] =
    AppConfig.load[IO].flatMap: config =>
      resources(config).use: (xa, backend) =>
        logging
          .forConsoleIo()
          .flatMap(logger => f(Context(config, xa, backend))(using logger))

  def validateTimestamp(str: String): ValidatedNel[String, DateTime] =
    Either
      .catchNonFatal(DateTime.parse(str))
      .leftMap(_.toString).toValidatedNel

  def timestampOpt(paramName: String): Opts[DateTime] =
    Opts.option[String](paramName, "Timestamp").mapValidated(validateTimestamp)
