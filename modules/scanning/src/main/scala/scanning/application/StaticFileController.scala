package scanning.application

import core.application.controller.Controller
import org.http4s.{ EntityDecoder, HttpRoutes, MediaType }
import org.http4s.headers.*
import org.http4s.dsl.Http4sDsl
import org.http4s.*
import cats.{ Monad, MonadError }
import cats.syntax.all.*
import cats.effect.kernel.Sync
import fs2.io.file.Files

object StaticFileController:
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]

  def make[F[_]: Monad: ThrowableMonadError: Sync: Files]: Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      def routes: HttpRoutes[F] = HttpRoutes.of[F]:
        case request @ GET -> Root / "static" / path =>
          val p = fs2.io.file.Path(s"./static/$path")
          StaticFile.fromPath(p, Some(request)).getOrElseF(NotFound())
