package infra

import scala.util.Try

import cats.*
import cats.implicits.*
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.client3.circe.*
import io.circe.Decoder
import sttp.model.Uri
import scala.concurrent.duration.*

case class RepositoryTreeFile(name: String, path: String) derives Decoder

type RepositoryTree = List[RepositoryTreeFile]

case class RepositoryFile(content: String) derives Decoder

trait GitlabApi[F[_]]:
  def getFile(
      id: String,
      branch: String,
      filePath: String
  ): F[Either[String, RepositoryFile]]

object GitlabApi:
  type RequestResult[F[_]] = ApplicativeError[F, Throwable]

  def make[F[_]: Applicative: RequestResult](
      backend: SttpBackend[F, WebSockets],
      host: String,
      token: Option[String]
  ): GitlabApi[F] =
    new GitlabApi[F]:
      def getFile(
          id: String,
          branch: String,
          filePath: String
      ): F[Either[String, RepositoryFile]] =
        val queryParams = Map(
          "ref"           -> branch,
          "private_token" -> token.getOrElse("")
        )
        val projectFileEndpoint =
          uri"https://${host}/api/v4/projects/${id}/repository/files/$filePath?$queryParams"

        basicRequest
          .get(projectFileEndpoint)
          .readTimeout(10.seconds)
          .response(asJson[RepositoryFile])
          .send(backend)
          .map(_.body.leftMap(buildErrorMessage(projectFileEndpoint)))

  def decodeContent(encodedContent: String): Either[Throwable, String] =
    Either.catchNonFatal(
      new String(java.util.Base64.getDecoder.decode(encodedContent))
    )

  private def buildErrorMessage(url: sttp.model.Uri)(
      exception: ResponseException[
        String,
        io.circe.Error
      ]
  ): String =
    s"url: ${url.toString}, ${exception.getMessage()}"
