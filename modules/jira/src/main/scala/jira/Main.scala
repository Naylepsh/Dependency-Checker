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
      val jira      = Jira.make[IO](config, backend, Template(""))
      val key       = ProjectKey("OPDDEV")
      val issueType = "Task"
      jira
        .createTicket(key, issueType, Map())
        .map: result =>
          println(s"result: $result")
        .as(ExitCode.Success)
