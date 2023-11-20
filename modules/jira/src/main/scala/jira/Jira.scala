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

type Summary = Summary.Type
object Summary extends Newtype[String]

case class Description(paragraphContent: List[Content])

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

case class TicketTemplate(
    summary: Template,
    description: Template
)
object TicketTemplate:
  def fromFiles(
      summaryFile: String,
      descriptionFile: String
  ): Either[String, TicketTemplate] =
    // Technically it should be wrapped in IO effect, practically idc
    (
      Either.catchNonFatal(scala.io.Source.fromFile(summaryFile).mkString),
      Either.catchNonFatal(scala.io.Source.fromFile(descriptionFile).mkString),
    ).tupled
      .map: (summary, description) =>
        TicketTemplate(Template(summary), Template(description))
      .leftMap(_.toString)

trait Jira[F[_]]:
  def createTicket(
      projectKey: ProjectKey,
      summary: Summary,
      description: Description,
      issueType: String
  ): F[Either[String, Unit]]
  def createTicket(
      projectKey: ProjectKey,
      issueType: String,
      templateVariables: Map[String, String]
  ): F[Either[String, Unit]]

object Jira:
  def make[F[_]: Monad](
      config: Config,
      backend: SttpBackend[F, WebSockets],
      template: TicketTemplate
  ): Jira[F] = new:
    given conv[T: Encoder]: Conversion[T, Json] with
      def apply(x: T): Json = x.asJson

    def createTicket(
        projectKey: ProjectKey,
        issueType: String,
        templateVariables: Map[String, String]
    ): F[Either[String, Unit]] =
      val body = Template
        .fill(template.description, templateVariables)
        .value
        .pipe(parse)
        .map: description =>
          val summary = Template.fill(template.summary, templateVariables).value
          Map[String, Json](
            "project"     -> Map("key" -> projectKey),
            "issuetype"   -> Map("name" -> issueType),
            "summary"     -> summary,
            "description" -> description
          ).asJson

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

    def createTicket(
        projectKey: ProjectKey,
        summary: Summary,
        description: Description,
        issueType: String
    ): F[Either[String, Unit]] =
      val body: Json = Map(
        "fields" -> Map[String, Json](
          "project"   -> Map("key" -> projectKey),
          "issuetype" -> Map("name" -> issueType),
          "summary"   -> summary,
          "description" -> Map[String, Json](
            "type"    -> "doc",
            "version" -> 1,
            "content" -> List(
              Map[String, Json](
                "type" -> "paragraph",
                "content" -> description
                  .paragraphContent
                  .map(_.toMessage)
                  .asJson
              )
            ).asJson
          )
        )
      )
      basicRequest
        .post(uri"${config.address}/rest/api/3/issue")
        .auth
        .basic(config.username.value, config.password.value)
        .body(body)
        .send(backend)
        .map(_.body.leftMap(_.toString).void)
