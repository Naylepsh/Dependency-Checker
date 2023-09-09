package upkeep.infra

import cats.effect.MonadCancelThrow
import cats.implicits.*
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import org.joda.time.DateTime
import upkeep.domain.*
import core.domain.project.ProjectScanConfigRepository

object ProjectRepository:
  def make[F[_]: MonadCancelThrow](xa: Transactor[F])
      : ProjectDependencyRepository[F, String] = new:
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
      ProjectRepositorySQL
        .getAffectedProjects(dependencyName, scanTimestamp)
        .to[List]
        .transact(xa)
        .map: raws =>
          raws.map: raw =>
            UpdateDependency(
              raw.gitlabId,
              raw.branch,
              raw.filePath,
              raw.name,
              raw.from,
              raw.to
            )

private object ProjectRepositorySQL:
  import persistence.sqlmappings.given

  case class RawAffectedProject(
      projectName: String,
      gitlabId: String,
      branch: String,
      filePath: String,
      name: String,
      from: String,
      to: String
  )
  object RawAffectedProject:
    given Read[RawAffectedProject] = Read[(
        String,
        String,
        String,
        String,
        String,
        String,
        String
    )].map: (pName, gitlabId, branch, groupName, name, from, to) =>
      RawAffectedProject(
        pName,
        gitlabId,
        branch,
        groupName.split(":").head,
        name,
        from,
        to
      )

  case class EnabledProject(gitlabId: String, branch: String)
  object EnabledProject:
    given Read[EnabledProject] = Read[(String, String)].map:
      (gitlabId, branch) =>
        EnabledProject(gitlabId, branch)

  def getAffectedProjects(
      dependencyName: String,
      scanTimestamp: DateTime
  ): Query0[RawAffectedProject] =
    // TODO: this query probably needs revisiting after the whole timestamp schema changed
    sql"""
    SELECT p.name,
           psc.gitlab_id,
           psc.branch,
           pd.group_name,
           $dependencyName,
           ds.current_version,
           ds.latest_version
    FROM project p 
    JOIN project_dependency pd pd.project_id = p.id
    JOIN project_scan_config psc ON psc.project_id = p.id
    JOIN dependency d on d.id = pd.dependency_id
    JOIN dependency_scan ds on ds.dependency_id = pd.dependency_id
    WHERE pd.timestamp = $scanTimestamp
          AND d.name = $dependencyName
          AND ds.current_version IS NOT NULL
          AND psc.enabled = true
    """.query[RawAffectedProject]

  def getLatestScansTimestamps(limit: Int): Query0[DateTime] =
    sql"""
    SELECT DISTINCT timestamp
    FROM dependency_scan
    ORDER BY timestamp DESC
    LIMIT $limit
    """.query[DateTime]
