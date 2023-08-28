package scanning.infra.packageindexes

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import core.domain.dependency.Dependency
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.httpclient.cats.HttpClientCatsBackend

class PypiSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  import PypiSpec.*

  "Pypi " - {
    "should parse package data" in:
      HttpClientCatsBackend.resource[IO]().use: backend =>
        Pypi(backend)
          .getDetails(django)
          .asserting:
            case Right(dependency) =>
              dependency.latestReleaseDate match
                case Some(latestReleaseDate) =>
                  dependency.releaseDate should be < latestReleaseDate
                case None =>
                  assert(false, "Did not extract latest release date")
            case Left(error) => assert(false, s"Did not expect error: $error")

    "should parse package with different date format" in:
      HttpClientCatsBackend.resource[IO]().use: backend =>
        Pypi(backend)
          .getDetails(django)
          .asserting(_.isRight shouldBe true)

    "should fail to parse package data of package with non-existing version" in:
      HttpClientCatsBackend.resource[IO]().use: backend =>
        Pypi(backend)
          .getDetails(djangoWithNonExistingVersion)
          .asserting(_.isRight shouldBe false)

    "should fail to parse package data of non-existing package" in:
      HttpClientCatsBackend.resource[IO]().use: backend =>
        Pypi(backend)
          .getDetails(nonExistingPackage)
          .asserting(_.isRight shouldBe false)
  }

object PypiSpec:
  val werkzeug                     = Dependency("werkzeug", Some("2.3.7"))
  val django                       = Dependency("django", Some("2.2.28"))
  val djangoWithNonExistingVersion = Dependency("django", Some("1337.42.0"))
  val nonExistingPackage           = Dependency("django-ognajd-foo-bar", None)
