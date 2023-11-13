package update

import cats.Monad
import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import core.controller.Controller
import io.circe.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.server.Router

import domain.{ UpdateDependency, UpdateService }

object controllers:
  object UpdateController:
    def make[F[_]: Monad: Concurrent](
        service: UpdateService[F]
    ): Controller[F] = new Controller[F] with Http4sDsl[F]:
      private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F]:
        case req @ POST -> Root =>
          req.as[UpdatePayload].flatMap: updateRequest =>
            service.update(updateRequest.toDomain).flatMap:
              case Left(error) => InternalServerError(error)
              case Right(_)    => Ok("Update submitted")

      val routes: HttpRoutes[F] = Router("api/update" -> httpRoutes)

    private case class UpdatePayload(
        projectName: String,
        dependencyName: String,
        filePath: String,
        fromVersion: String,
        toVersion: String
    ) derives Decoder:
      def toDomain: UpdateDependency =
        UpdateDependency(
          projectName,
          dependencyName,
          filePath,
          fromVersion,
          toVersion
        )
    private object UpdatePayload:
      implicit def decoder[F[_]: Concurrent]: EntityDecoder[F, UpdatePayload] =
        jsonOf[F, UpdatePayload]
