package jira

import core.Newtype
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.client3.circe.*

type ProjectKey = ProjectKey.Type
object ProjectKey extends Newtype[String]

type Summary = Summary.Type
object Summary extends Newtype[String]

type Description = Description.Type
object Description extends Newtype[String]

enum IssueType:
  case Bug, Task, Story

trait Jira[F[_]]:
  def createTicket(
      projectKey: ProjectKey,
      summary: Summary,
      description: Description,
      issueType: IssueType
  ): F[Either[String, Unit]]

object Jira:
  def make[F[_]](backend: SttpBackend[F, WebSockets]): Jira[F] = new:
    def createTicket(
        projectKey: ProjectKey,
        summary: Summary,
        description: Description,
        issueType: IssueType
    ): F[Either[String, Unit]] = ???
