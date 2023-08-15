package core.infra.persistance

import java.util.UUID

import core.domain.project.ProjectScanConfigRepository
import cats.implicits.*
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.effect.std.UUIDGen
import cats.effect.std.UUIDGen.randomUUID
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import doobie.util.transactor.Transactor
import core.domain.project.{ Project, ProjectScanConfig }
import core.domain.dependency.DependencySource
import core.domain.dependency.DependencySource.{ TomlSource, TxtSource }

object ProjectScanConfigRepository:
  def make[F[_]: MonadCancelThrow: UUIDGen](xa: Transactor[F])
      : ProjectScanConfigRepository[F] = new:
    def all: F[List[ProjectScanConfig]] =
      for
        configs <- SQL.allConfigs.to[List].transact(xa)
        configIds = NonEmptyList.fromList(configs.map(_.configId))
        txtSources <- configIds
          .map(SQL.getTxtSources.andThen(_.to[List].transact(xa)))
          .getOrElse(List.empty.pure)
        tomlSources <- configIds
          .map(SQL.getTomlSources.andThen(_.to[List].transact(xa)))
          .getOrElse(List.empty.pure)
      yield SQL.RawConfig.toDomain(configs, txtSources, tomlSources)

    def save(config: ProjectScanConfig): F[UUID] =
      for
        configId <- randomUUID
        sources <- config.sources.traverse: source =>
          randomUUID.map(id => id -> source)
        _ <- 
          val inserts = for
            _ <- SQL.insertConfig(configId, config).run
            // terrible performance, but it doesn't get called that much, so low priority
            // TODO: Improve perf. by updating in batch
            _ = sources.traverse:
              case (id, txt @ TxtSource(_)) => SQL.insertTxtSource(id, configId, txt).run
              case (id, toml @ TomlSource(_, _)) => SQL.insertTomlSource(id, configId, toml).run
          yield ()
          inserts.transact(xa)
      yield configId

private object SQL:
  import sqlmappings.given

  private[persistance] case class RawConfig(
      projectName: String,
      configId: UUID,
      gitlabId: Int,
      enabled: Boolean,
      branch: String
  )
  private[persistance] object RawConfig:
    def toDomain(
        configs: List[RawConfig],
        txtSources: List[RawTxtSource],
        tomlSources: List[RawTomlSource]
    ): List[ProjectScanConfig] =
      val txtMap = txtSources
        .groupBy(_.configId)
        .map: (projectId, sources) =>
          projectId -> sources.map: source =>
            TxtSource(source.path)
        .toMap
      val tomlMap = tomlSources
        .groupBy(_.configId)
        .map: (projectId, sources) =>
          projectId -> sources.map: source =>
            TomlSource(source.path, source.group)
        .toMap
      configs.map: config =>
        val sources = txtMap.getOrElse(config.configId, List.empty)
          ++ tomlMap.getOrElse(config.configId, List.empty)
        ProjectScanConfig(
          Project(config.gitlabId.toString, config.projectName),
          sources,
          config.enabled,
          config.branch
        )

  private[persistance] case class RawTxtSource(configId: UUID, path: String)
  private[persistance] case class RawTomlSource(
      configId: UUID,
      path: String,
      group: Option[String]
  )

  def allConfigs =
    sql"""
      SELECT projectName, configId, gitlabId, enabled, branch
      FROM projectScanConfig
      """.query[RawConfig]

  def getTxtSources(projectIds: NonEmptyList[UUID]) =
    val s =
      sql"""
      SELECT projectId, path
      FROM txtSource
      WHERE """ ++ Fragments.in(fr"configId", projectIds)
    s.query[RawTxtSource]

  def getTomlSources(projectIds: NonEmptyList[UUID]) =
    val s =
      sql"""
      SELECT projectId, path, group
      FROM tomlSource
      WHERE """ ++ Fragments.in(fr"configId", projectIds)
    s.query[RawTomlSource]

  def insertConfig(id: UUID, config: ProjectScanConfig) =
    sql"""
    INSERT INTO projectScanConfig (id, gitlabId, projectName, enabled, branch)
    VALUES ($id, ${config.project.id}, ${config.project.name}, ${config.enabled}, ${config.branch})
    """.update

  def insertTxtSource(id: UUID, configId: UUID, source: TxtSource) =
    sql"""
    INSERT INTO txtSource (id, configId, path)
    VALUES ($id, $configId, ${source.path})
    """.update

  def insertTomlSource(id: UUID, configId: UUID, source: TomlSource) =
    sql"""
    INSERT INTO tomlSource (id, configId, path, group)
    VALUES ($id, $configId, ${source.path}, ${source.group})
    """.update
