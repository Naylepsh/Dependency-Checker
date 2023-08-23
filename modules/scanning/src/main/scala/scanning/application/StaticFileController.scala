package scanning.application

import cats.effect.kernel.Sync
import cats.syntax.all.*
import cats.{ Monad, MonadError }
import core.application.controller.Controller
import fs2.io.file.Files
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*

object StaticFileController:
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]

  def make[F[_]: Monad: ThrowableMonadError: Sync: Files]: Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      def routes: HttpRoutes[F] = HttpRoutes.of[F]:
        case request @ GET -> Root / "static" / path =>
          val p = fs2.io.file.Path(s"./static/$path")
          StaticFile.fromPath(p, Some(request)).getOrElseF(NotFound())
