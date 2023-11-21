package jira

import cats.effect.*
import cats.syntax.all.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend

object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    val config = Config(
      username = Username(???),
      password = Password(???),
      address = Address(???)
    )
    HttpClientCatsBackend.resource[IO]().use: backend =>
      val jira = Jira.make[IO](
        config,
        backend,
        Template
          .fromFile("./jira-template.example.json")
          .right
          .getOrElse(Template("{}"))
      )
      val key       = ProjectKey(???)
      val issueType = "Task"
      val variables = Map(
        "projectName"     -> "test-project",
        "dependencyName"  -> "test-dependency",
        "fromVersion"     -> "1.2.3",
        "toVersion"       -> "1.2.4",
        "mergeRequestUrl" -> "http://localhost:8000/path/to/test-project/-/merge_requests/42"
      )
      jira
        .createTicket(key, issueType, variables)
        .map: result =>
          println(s"result: $result")
        .as(ExitCode.Success)
