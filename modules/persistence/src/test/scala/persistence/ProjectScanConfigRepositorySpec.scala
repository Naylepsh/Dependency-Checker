package persistence

import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Tag
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import core.infra.resources.database.*
import core.domain.project.ProjectScanConfig
import core.domain.project.Project
import core.domain.dependency.DependencySource
import persistance.ProjectScanConfigRepository

object DatabaseTest extends Tag("DatabaseTest")

class ProjectScanConfigRepositorySpec extends AsyncFreeSpec with AsyncIOSpec
    with Matchers:
  import ProjectScanConfigRepositorySpec.*

  "Save and find saved project configs" taggedAs (DatabaseTest) in:
    transactor.use: xa =>
      val repository = ProjectScanConfigRepository.make(xa)

      for
        _       <- repository.save(txtScanConfig)
        _       <- repository.save(tomlScanConfig)
        _       <- repository.save(mixedScanConfig)
        configs <- repository.all
      yield configs should (
        have length (3)
          // NOTE: contain is wonky and requires the projectScanConfig list to be in exact order
          and contain(txtScanConfig)
          and contain(tomlScanConfig)
          and contain(mixedScanConfig)
      )

object ProjectScanConfigRepositorySpec:
  val config =
    Config("sqlite:///home/naylepsh/dev/sentinel/db.test.sqlite", "", "")
  val transactor = makeSqliteTransactorResource[IO](config).evalTap: xa =>
    val freshStart =
      for
        _ <- sql"DELETE FROM projectScanConfig".update.run
        _ <- sql"DELETE FROM txtSource".update.run
        _ <- sql"DELETE FROM tomlSource".update.run
      yield xa
    freshStart.transact(xa)

  val txtScanConfig = ProjectScanConfig(
    project = Project(id = "1", name = "foo"),
    sources = List(
      DependencySource.TxtSource("requirements.txt"),
      DependencySource.TxtSource("requirements.test.txt")
    ),
    enabled = false,
    branch = "master"
  )
  val tomlScanConfig = ProjectScanConfig(
    project = Project(id = "2", name = "bar"),
    sources = List(
      DependencySource.TomlSource("pyproject.toml", "dependencies".some),
      DependencySource.TomlSource("pyproject.toml", "dev-dependencies".some)
    ),
    enabled = true,
    branch = "stage"
  )
  val mixedScanConfig = ProjectScanConfig(
    project = Project(id = "3", name = "baz"),
    sources = List(
      DependencySource.TxtSource("requirements.txt"),
      DependencySource.TomlSource("pyproject.toml", "dependencies".some)
    ),
    enabled = true,
    branch = "dev"
  )
