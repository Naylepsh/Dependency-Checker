package update.services

import java.net.URI

import cats.syntax.all.*
import cats.{ Applicative, Monad }
import jira.*
import update.domain.*

type GetTicketData[F[_]] = (
    UpdateDependencyDetails,
    URI
) => F[(ProjectKey, Summary, Description, String)]
object GetTicketData:
  def default[F[_]: Applicative](key: ProjectKey, issueType: String)(
      request: UpdateDependencyDetails,
      mrUrl: URI
  ): F[(ProjectKey, Summary, Description, String)] =
    val summary = Summary(
      s"Bump ${request.dependencyName} from ${request.fromVersion} to ${request.toVersion}"
    )
    val description = Description(List(Content.Link(mrUrl)))

    Applicative[F].pure((key, summary, description, issueType))

trait JiraNotificationService[F[_]]:
  def notify(
      request: UpdateDependencyDetails,
      mergeRequestUrl: URI
  ): F[Either[String, Unit]]

object JiraNotificationService:

  def make[F[_]: Monad](
      jira: Jira[F],
      getTicketData: GetTicketData[F]
  ): JiraNotificationService[F] = new:
    def notify(
        request: UpdateDependencyDetails,
        mergeRequestUrl: URI
    ): F[Either[String, Unit]] =
      getTicketData(request, mergeRequestUrl).flatMap:
        (key, summary, description, issueType) =>
          jira.createTicket(key, summary, description, issueType)

  def noop[F[_]: Applicative]: JiraNotificationService[F] = new:
    def notify(
        request: UpdateDependencyDetails,
        mergeRequestUrl: URI
    ): F[Either[String, Unit]] = Applicative[F].pure(().asRight)
