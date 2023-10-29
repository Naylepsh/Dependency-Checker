package update

import cats.Monad
import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.server.Router
import domain.UpdateService
import core.controller.Controller

object controllers:
  object UpdateController:
    def make[F[_]: Monad: Concurrent](
        service: UpdateService[F]
    ): Controller[F] = new Controller[F] with Http4sDsl[F]:
      private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F]:
        ???

      val routes: HttpRoutes[F] = Router("update" -> httpRoutes)
