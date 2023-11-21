package jira

import java.net.URI

import scala.util.chaining.*

import cats.Monad
import cats.implicits.*
import core.Newtype
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.client3.circe.*

type ProjectKey = ProjectKey.Type
object ProjectKey extends Newtype[String]

type Username = Username.Type
object Username extends Newtype[String]

type Password = Password.Type
object Password extends Newtype[String]

type Address = Address.Type
object Address extends Newtype[String]

case class Config(username: Username, password: Password, address: Address)

sealed trait Content:
  def toMessage: Map[String, Json]
object Content:
  case class Text(value: String) extends Content:
    def toMessage: Map[String, Json] =
      Map("text" -> value.asJson, "type" -> "text".asJson)

  case class Link(url: URI) extends Content:
    def toMessage: Map[String, Json] =
      Map("type" -> "inlineCard".asJson, "attrs" -> Map("url" -> url).asJson)

type Template = Template.Type
object Template extends Newtype[String]:
  def fill(template: Template, variables: Map[String, String]): Template =
    var filled = template.value
    variables.foreach: (key, value) =>
      filled = filled.replace(s"{{$key}}", value)
    Template(filled)

  def fromFile(path: String): Either[String, Template] =
    Either
      .catchNonFatal(scala.io.Source.fromFile(path).mkString)
      .map(Template(_))
      .leftMap(_.toString)

trait Jira[F[_]]:
  def createTicket(
      projectKey: ProjectKey,
      issueType: String,
      templateVariables: Map[String, String]
  ): F[Either[String, Unit]]

object Jira:
  def make[F[_]: Monad](
      config: Config,
      backend: SttpBackend[F, WebSockets],
      template: Template
  ): Jira[F] = new:

    def createTicket(
        projectKey: ProjectKey,
        issueType: String,
        templateVariables: Map[String, String]
    ): F[Either[String, Unit]] =
      val body = Template
        .fill(
          template,
          templateVariables + ("projectKey" -> projectKey.value) + ("issueType" -> issueType)
        )
        .value
        .pipe(parse)

      body match
        case Left(error) => error.toString.asLeft.pure
        case Right(body) =>
          basicRequest
            .post(uri"${config.address}/rest/api/3/issue")
            .auth
            .basic(config.username.value, config.password.value)
            .body(body)
            .send(backend)
            .map(_.body.leftMap(_.toString).void)

