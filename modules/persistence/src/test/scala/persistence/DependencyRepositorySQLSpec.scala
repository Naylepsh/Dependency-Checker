package persistence

import org.scalatest.*
import cats.effect.{ IO, Resource }
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import doobie.*
import doobie.implicits.*
import core.infra.resources.database.*
import core.application.config.AppConfig
import DependencyRepository.{DependencyRepositorySQL => SQL}

class DependencyRepositorySQLSpec extends freespec.AnyFreeSpec
    with matchers.must.Matchers
    with doobie.scalatest.IOChecker:

  override val colors = doobie.util.Colors.None // just for docs

  val transactor = AppConfig
    .load[IO]
    .map: config =>
      Transactor.fromDriverManager[IO](
        driver = "org.sqlite.JDBC",
        url = s"jdbc:${config.database.path}",
        user = config.database.username,
        pass = config.database.password
      )
    .unsafeRunSync()

  /*
   * Looks like doobie checking has problems with fragments.
   * Running the query below through the checker (or any Fragments.in really)
   * ends up with `Cannnot read the array.length because "this.batch" is null`
   */
  // "Check findLatestReleases SQL" taggedAs (DatabaseTest) in:
  //   check(DependencyRepositorySQL.findLatestReleases(NonEmptyList.of(
  //     "foo",
  //     "bar"
  //   )))
  /*
   * And bulk updates don't work either ;)
   */
  // 
  // "Check vulnerability insertion" taggedAs (DatabaseTest) in:
  //   check(SQL.insertVulnerabilities)
  //
  // "Check dependency insertion" taggedAs (DatabaseTest) in:
  //   check(SQL.insertDependencies)
