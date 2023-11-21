package update.services

import java.net.URI

import cats.syntax.all.*
import cats.{ Applicative, Monad }
import jira.*
import update.domain.*

trait JiraNotificationService[F[_]]:
  def notify(
      request: UpdateDependencyDetails,
      mergeRequestUrl: URI
  ): F[Either[String, Unit]]

object JiraNotificationService:

  def make[F[_]: Monad](
      jira: Jira[F],
      projectKey: ProjectKey,
      issueType: String
  ): JiraNotificationService[F] = new:
    def notify(
        request: UpdateDependencyDetails,
        mergeRequestUrl: URI
    ): F[Either[String, Unit]] =
      val variables = Map(
        "projectName"     -> request.projectName,
        "dependencyName"  -> request.dependencyName,
        "fromVersion"     -> request.fromVersion,
        "toVersion"       -> request.toVersion,
        "mergeRequestUrl" -> mergeRequestUrl.toString,
      )
      jira.createTicket(projectKey, issueType, variables)

  def noop[F[_]: Applicative]: JiraNotificationService[F] = new:
    def notify(
        request: UpdateDependencyDetails,
        mergeRequestUrl: URI
    ): F[Either[String, Unit]] = Applicative[F].pure(().asRight)
