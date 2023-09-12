package scanning.application

import cats.effect.kernel.Sync
import cats.syntax.all.*
import cats.{ Monad, MonadError }
import core.controller.Controller
import fs2.io.file.Files
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.syntax.all.*

object RootController:
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]

  def make[F[_]: Monad: ThrowableMonadError: Sync: Files]: Controller[F] =
    new Controller[F] with Http4sDsl[F]:
      def routes: HttpRoutes[F] = HttpRoutes.of[F]:
        case GET -> Root =>
          TemporaryRedirect(Location(uri"/project"))
