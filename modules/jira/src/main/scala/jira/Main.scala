package jira

import cats.effect.*
import cats.syntax.all.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend

object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    val config = Config(
      username = Username("???"),
      password = Password("???"),
      address = Address("???")
    )
    HttpClientCatsBackend.resource[IO]().use: backend =>
      val jira        = Jira.make[IO](config, backend)
      val key         = ProjectKey("OPDDEV")
      val summary     = Summary("Summary test")
      val description = Description(List.empty)
      val issueType   = "Task"
      jira
        .createTicket(key, summary, description, issueType)
        .map: result =>
          println(s"result: $result")
        .as(ExitCode.Success)
