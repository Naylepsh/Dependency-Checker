package upkeep.infra

import cats.effect.MonadCancelThrow
import cats.implicits.*
import upkeep.domain.*
import core.domain.registry.RegistryRepository
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import org.joda.time.DateTime
import core.domain.registry.Registry

object ProjectRepository:
  def make[F[_]: MonadCancelThrow](
      xa: Transactor[F],
      registryRepository: RegistryRepository[F]
  ): ProjectDependencyRepository[F, String] = new:
    override def getAffectedProjects(dependencyName: String)
        : F[List[UpdateDependency[String]]] =
      ProjectRepositorySQL
        .getLatestScansTimestamps(1)
        .option
        .transact(xa)
        .flatMap {
          case Some(timestamp) => getAffectedProjects(dependencyName, timestamp)
          case None            => List.empty.pure
        }

    def getAffectedProjects(
        dependencyName: String,
        scanTimestamp: DateTime
    ): F[List[UpdateDependency[String]]] =
      for
        raws <- ProjectRepositorySQL.getAffectedProjects(
          dependencyName,
          scanTimestamp
        ).to[List].transact(xa)
        projects <- registryRepository
          .get()
          .map(_.map(_.projects).getOrElse(List.empty))
      yield raws.flatMap(raw =>
        projects.find(_.name == raw.projectName).map: project =>
          UpdateDependency(
            project.id,
            project.branch,
            raw.filePath,
            raw.name,
            raw.from,
            raw.to
          )
      )

object ProjectRepositorySQL:
  import core.infra.persistance.sqlmappings.given

  case class RawAffectedProject(
      projectName: String,
      filePath: String,
      name: String,
      from: String,
      to: String
  )

  given Read[RawAffectedProject] = Read[(
      String,
      String,
      String,
      String,
      String
  )].map((pName, groupName, name, from, to) =>
    RawAffectedProject(pName, groupName.split(":").head, name, from, to)
  )

  def getAffectedProjects(
      dependencyName: String,
      scanTimestamp: DateTime
  ): Query0[RawAffectedProject] =
    sql"""
    SELECT pd.projectName,
            pd.groupName,
            $dependencyName,
            ds.currentVersion,
            ds.latestVersion
    FROM projectDependency pd
    JOIN dependency d on d.id = pd.dependencyId
    JOIN dependencyScan ds on ds.dependencyId = pd.dependencyId
    WHERE pd.timestamp = $scanTimestamp
          AND d.name = $dependencyName
          AND ds.currentVersion IS NOT NULL
    """.query[RawAffectedProject]

  def getLatestScansTimestamps(limit: Int): Query0[DateTime] =
    sql"""
    SELECT DISTINCT timestamp
    FROM dependencyScan
    ORDER BY timestamp DESC
    LIMIT $limit
    """.query[DateTime]
