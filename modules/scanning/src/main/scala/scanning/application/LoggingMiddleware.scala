package scanning.application

import org.http4s.*
import org.legogroup.woof.{ *, given }
import cats.Monad
import cats.syntax.all.*
import cats.data.{ Kleisli, OptionT }

object LoggingMiddleware:
  def wrap[F[_]: Logger: Monad](routes: HttpRoutes[F]): HttpRoutes[F] =
    Kleisli: (req: Request[F]) =>
      for
        _   <- OptionT.liftF(Logger[F].info(s"${req.method} ${req.uri}"))
        res <- routes(req)
      yield res

