import cats.effect.*
import com.comcast.ip4s.*
import org.http4s.HttpApp
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.defaults.Banner
import org.legogroup.woof.{ *, given }
import config.ServerConfig

trait HttpServer[F[_]]:
  def newEmber(config: ServerConfig, app: HttpApp[F]): Resource[F, Server]

object HttpServer:
  def apply[F[_]: HttpServer]: HttpServer[F] = summon

  given given_HttpServer[F[_]: Async: Logger]: HttpServer[F] with
    def newEmber(config: ServerConfig, app: HttpApp[F]): Resource[F, Server] =
      EmberServerBuilder
        .default[F]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(app)
        .build
        .evalTap(showEmberBanner)

  private def showEmberBanner[F[_]: Logger](server: Server) =
    Logger[F].info(
      s"\n${Banner.mkString("\n")}\nHTTP Server started at ${server.address}"
    )
