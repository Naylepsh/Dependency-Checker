package persistence

import java.time.Instant
import java.util.UUID

import cats.*
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.effect.kernel.Sync
import cats.effect.std.UUIDGen
import cats.effect.std.UUIDGen.randomUUID
import cats.free.Free
import cats.implicits.*
import core.domain.dependency.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import org.joda.time.DateTime
import doobie.free.connection.ConnectionOp
import doobie.util.update.Update

object DependencyRepository:
  def make[F[_]: MonadCancelThrow: UUIDGen](xa: Transactor[F])
      : DependencyRepository[F] = new:
    import DependencyRepositorySQL.*

    def delete(timestamps: NonEmptyList[DateTime]): F[Unit] =
      deleteByTimestamps(timestamps).run.transact(xa).void

    def save(
        dependencies: List[DependencyReport],
        timestamp: DateTime
    ): F[List[ExistingDependency]] =
      for
        existingDependencies <-
          NonEmptyList
            .fromList(dependencies.map(_.name))
            .map: names =>
              findDependencies(names).to[List].transact(xa)
            .getOrElse(List.empty.pure)
        /*
         * existingDeps and newDeps make the existingDependencies traversal twice.
         * TODO: Improve it
         */
        existingDeps <- dependencies
          .map: dependency =>
            existingDependencies
              .find: (id, name) =>
                name == dependency.name
              .map: (id, name) =>
                id -> dependency
          .collect:
            case Some(id, dependency) => id -> dependency
          .traverse: (id, dependency) =>
            ResultToSave(id, dependency)
        newDeps <- dependencies
          .map: dependency =>
            if existingDependencies
                .find: (id, name) =>
                  name == dependency.name
                .isDefined
            then None
            else dependency.some
          .collect:
            case Some(dependency) => dependency
          .traverse: dependency =>
            ResultToSave(dependency)
        latests <- dependencies.traverse: dependency =>
          ResultToSave.forLatest(dependency)
        allDeps = newDeps ++ existingDeps
        _ <-
          val changes =
            NonEmptyList
              .fromList(latests)
              .map: latests =>
                deleteLatest(latests.map(_.dependency.name)).run
              .getOrElse(connectionIONoop)
              *> insertDependencies
                .updateMany((newDeps ++ latests).map(_.dependency))
              *> insertVulnerabilities
                .updateMany((allDeps).flatMap(_.vulnerabilities))
          changes.transact(xa)
      yield allDeps
        .map: result =>
          ExistingDependency(result.dependency.id, result.dependency.name)

    def findLatestReleases(ids: List[UUID]): F[List[DependencyLatestRelease]] =
      NonEmptyList
        .fromList(ids)
        .map: ids =>
          DependencyRepositorySQL
            .findLatestReleases(ids)
            .to[List]
            .transact(xa)
        .getOrElse(List.empty.pure)

  private[persistence] case class ExistingVulnerability(
      id: UUID,
      dependencyScanId: UUID,
      name: String
  )
  private[persistence] case class DependencyToSave(
      id: UUID,
      name: String,
      version: Option[String],
      releaseDate: Option[DateTime],
      notes: Option[String]
  )

  private[persistence] case class ResultToSave(
      dependency: DependencyToSave,
      vulnerabilities: List[ExistingVulnerability]
  )
  private[persistence] object ResultToSave:
    def apply[F[_]: UUIDGen: Monad](
        dependencyId: UUID,
        report: DependencyReport
    ): F[ResultToSave] =
      val dependency = DependencyToSave(
        dependencyId,
        report.name,
        report.currentVersion,
        report.currentVersionReleaseDate,
        report.notes
      )
      report.vulnerabilities
        .traverse: vulnerability =>
          randomUUID.map: id =>
            ExistingVulnerability(id, dependencyId, vulnerability)
        .map: vulnerabilities =>
          ResultToSave(dependency, vulnerabilities)

    def apply[F[_]: UUIDGen: Monad](report: DependencyReport): F[ResultToSave] =
      randomUUID.flatMap: dependencyId =>
        ResultToSave(dependencyId, report)

    def forLatest[F[_]: UUIDGen: Monad](report: DependencyReport)
        : F[ResultToSave] =
      randomUUID.map: dependencyId =>
        val dependency = DependencyToSave(
          dependencyId,
          report.name,
          report.latestVersion.some,
          report.latestReleaseDate,
          "LATEST".some
        )
        new ResultToSave(dependency, List.empty)

  private[persistence] object DependencyRepositorySQL:
    import sqlmappings.given

    val insertDependencies =
      val sql = """
        INSERT INTO dependency (id, name, version, release_date, notes)
        VALUES (?, ?, ?, ?, ?)
      """
      Update[DependencyToSave](sql)

    val insertVulnerabilities: Update[ExistingVulnerability] =
      val sql = """
          INSERT INTO vulnerability (id, dependency_id, name)
          VALUES (?, ?, ?)
        """
      Update[ExistingVulnerability](sql)

    def deleteByTimestamps(timestamps: NonEmptyList[DateTime]): Update0 =
      (sql"""
      DELETE 
      FROM dependency
      WHERE """ ++ Fragments.in(fr"timestamp", timestamps)).update

    def findDependencies(names: NonEmptyList[String]): Query0[(UUID, String)] =
      (sql"""
      SELECT id, name
      FROM dependency
      WHERE """ ++ Fragments.in(fr"name", names)).query[(UUID, String)]

    def deleteLatest(names: NonEmptyList[String]): Update0 =
      (sql"""
      DELETE 
      FROM dependency
      WHERE notes = 'LATEST'
      AND """ ++ Fragments.in(fr"name", names)).update

    // TODO: Move this to scan result repo? Or expose it from Dependency Repo
    def findLatestReleases(ids: NonEmptyList[UUID])
        : Query0[DependencyLatestRelease] =
      val select =
        sql"""
        SELECT name, version, MAX(release_date) AS latestReleaseDate
        FROM dependency 
        WHERE name IN (
          SELECT name
          FROM dependency
          WHERE """
          ++ Fragments.in(fr"id", ids)
          ++ sql"""
          ) 
          AND release_date IS NOT NULL
          AND version IS NOT NULL
          GROUP BY name
          """
      select.query[DependencyLatestRelease]

    def findNames(ids: NonEmptyList[UUID]): Query0[String] =
      ???

    val connectionIONoop = Free.pure[ConnectionOp, Unit](())
