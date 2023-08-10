package gitlab

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.httpclient.cats.HttpClientCatsBackend

class GitlabApiSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  import GitlabApiSpec.*

  "Requesting a file" - {
    "returns the file content" in {
      HttpClientCatsBackend.resource[IO]().use { backend =>
        GitlabApi.make(backend, gitlabHost, token)
          .getFile(
            gitlabId,
            gitlabMainBranch,
            gitlabExampleFilePath
          )
          .asserting(_.flatMap(file =>
            GitlabApi.decodeContent(file.content).map(
              _.contains(partialExampleFileContent)
            )
          ) shouldBe Right(true))
      }
    }

    "fails when a project cannot be found" in {
      HttpClientCatsBackend.resource[IO]().use { backend =>
        GitlabApi.make(backend, gitlabHost, token)
          .getFile(
            "0",
            gitlabMainBranch,
            gitlabExampleFilePath
          )
          .asserting(_.isLeft shouldBe true)
      }
    }

    "fails when a branch cannot be found" in {
      HttpClientCatsBackend.resource[IO]().use { backend =>
        GitlabApi.make(backend, gitlabHost, token)
          .getFile(
            gitlabId,
            "yabba-dabba-doooo",
            gitlabExampleFilePath
          )
          .asserting(_.isLeft shouldBe true)
      }
    }

    "fails when a path cannot be found" in {
      HttpClientCatsBackend.resource[IO]().use { backend =>
        GitlabApi.make(backend, gitlabHost, token)
          .getFile(
            gitlabId,
            gitlabMainBranch,
            "path/to/non/existing/file"
          )
          .asserting(_.isLeft shouldBe true)
      }
    }
  }

object GitlabApiSpec:
  val gitlabHost                = "gitlab.com"
  val token                     = None
  val gitlabId                  = "13083"
  val gitlabMainBranch          = "master"
  val gitlabExampleFilePath     = ".gitlab/issue_templates/Experiment Idea.md"
  val partialExampleFileContent = "Hypothesis"
