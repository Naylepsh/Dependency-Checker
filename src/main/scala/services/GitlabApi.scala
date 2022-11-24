package services

import upickle.default.{ReadWriter => RW, macroRW}
import utils.json
import cats._
import cats.implicits._
import scala.util.Try

object responses {
  case class RepositoryTreeFile(name: String, path: String)
  object RepositoryTreeFile {
    given RW[RepositoryTreeFile] = macroRW
  }

  type RepositoryTree = List[RepositoryTreeFile]

  case class RepositoryFile(content: String)
  object RepositoryFile {
    given RW[RepositoryFile] = macroRW
  }

}

import responses._

trait GitlabApi[F[_]] {
  def getFile(
      id: String,
      branch: String,
      filePath: String
  ): F[Either[Throwable, RepositoryFile]]
}

object GitlabApi {

  import responses._

  type RequestResult[F[_]] = ApplicativeError[F, Throwable]
  def make[F[_]: Applicative: RequestResult](
      host: String,
      token: Option[String]
  ): GitlabApi[F] =
    new GitlabApi[F] {
      def getFile(
          id: String,
          branch: String,
          filePath: String
      ): F[Either[Throwable, RepositoryFile]] = Applicative[F].pure {
        Try {
          val response = requests.get(
            s"https://${host}/api/v4/projects/${id}/repository/files/$filePath",
            params = Map(
              "ref" -> branch,
              "private_token" -> token.getOrElse("")
            )
          )
          json.parse[RepositoryFile](response.text())
        }.toEither
      }
    }
}
