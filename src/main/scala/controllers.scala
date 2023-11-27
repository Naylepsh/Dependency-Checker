import cats.data.{ Kleisli, OptionT }
import cats.effect.kernel.Sync
import cats.syntax.all.*
import cats.{ Monad, MonadError, MonadThrow }
import core.controller.Controller
import fs2.io.file.Files
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.server.Router
import org.http4s.syntax.all.*
import org.legogroup.woof.{ *, given }

object controllers:
  object RootController:
    def make[F[_]: MonadThrow: Sync: Files]: Controller[F] =
      new Controller[F] with Http4sDsl[F]:
        val routes: HttpRoutes[F] = HttpRoutes.of[F]:
          case GET -> Root =>
            TemporaryRedirect(Location(uri"/project"))

  object StaticFileController:
    def make[F[_]: MonadThrow: Sync: Files]: Controller[F] =
      new Controller[F] with Http4sDsl[F]:
        val routes: HttpRoutes[F] = HttpRoutes.of[F]:
          case request @ GET -> Root / "static" / path =>
            val p = fs2.io.file.Path(s"./static/$path")
            StaticFile.fromPath(p, Some(request)).getOrElseF(NotFound())

  object LoggingMiddleware:
    def wrap[F[_]: Logger: Monad: MonadThrow](routes: HttpRoutes[F])
        : HttpRoutes[F] = make(routes).routes

    def make[F[_]: Logger: Monad: MonadThrow](service: HttpRoutes[F])
        : Controller[F] =
      new Controller[F] with Http4sDsl[F]:
        val routes = Kleisli: (req: Request[F]) =>
          // Note that it won't log "404 Not Found" for unregistered routes
          for
            _ <- OptionT.liftF(Logger[F].info(s"${req.method} ${req.uri}"))
            res <- service(req).handleErrorWith: error =>
              OptionT.liftF:
                Logger[F].error(error.toString)
                  *> InternalServerError("Oops, something went wrong")
            _ <- OptionT.liftF:
              Logger[F].info(s"${req.method} ${req.uri} - ${res.status}")
          yield res

  object HealthcheckController:
    def make[F[_]: MonadThrow]: Controller[F] =
      new Controller[F] with Http4sDsl[F]:
        val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F]:
          case GET -> Root => Ok()

        val routes: HttpRoutes[F] = Router("api/v1/healtcheck" -> httpRoutes)
