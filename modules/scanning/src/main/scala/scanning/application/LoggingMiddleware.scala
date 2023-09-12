package scanning.application

import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.legogroup.woof.{ *, given }
import cats.{ Monad, MonadThrow }
import cats.syntax.all.*
import cats.data.{ Kleisli, OptionT }
import core.controller.Controller

object LoggingMiddleware:
  def wrap[F[_]: Logger: Monad: MonadThrow](routes: HttpRoutes[F])
      : HttpRoutes[F] = make(routes).routes

  def make[F[_]: Logger: Monad: MonadThrow](service: HttpRoutes[F])
      : Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      val routes = Kleisli: (req: Request[F]) =>
        for
          _ <- OptionT.liftF(Logger[F].info(s"${req.method} ${req.uri}"))
          res <- service(req).handleErrorWith: error =>
            OptionT.liftF:
              Logger[F].error(error.toString)
                *> InternalServerError("Oops, something went wrong")
        yield res
