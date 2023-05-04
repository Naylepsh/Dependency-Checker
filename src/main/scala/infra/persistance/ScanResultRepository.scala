package infra.persistance

import doobie.util.transactor.Transactor
import domain.project.ScanResultRepository
import domain.project.ScanResult
import domain.dependency.DependencyRepository
import cats.implicits.*
import cats.effect.MonadCancelThrow
import org.joda.time.DateTime
import java.util.UUID
import doobie.*
import doobie.implicits.*
import doobie.util.query.*
import org.legogroup.woof.{ *, given }

object ScanResultRepository:
  def make[F[_]: MonadCancelThrow: Logger](
      xa: Transactor[F],
      dependencyRepository: DependencyRepository[F]
  ): ScanResultRepository[F] =
    new ScanResultRepository[F]:
      import ScanResultRepositorySQL.*

      override def save(
          results: List[ScanResult],
          timestamp: DateTime
      ): F[Unit] =
        results.traverse(result =>
          result.dependenciesReports.traverse(group =>
            for
              _ <- Logger[F].debug(
                s"Saving ${group.items.length} dependencies & vulnerabilities"
              )
              dependencies <- dependencyRepository.save(group.items, timestamp)
              _ <- Logger[F].debug(
                s"Saving ${dependencies.length} projectDependencies"
              )
              records = dependencies.map(dependency =>
                ProjectDependency(
                  timestamp,
                  result.project.name,
                  group.groupName,
                  dependency.id
                )
              )
              _ <- insertMany(records).transact(xa)
            yield ()
          )
        ).void

  case class ProjectDependency(
      timestamp: DateTime,
      projectName: String,
      groupName: String,
      dependencyId: UUID
  )

  object ScanResultRepositorySQL:
    import sqlmappings.given

    def insertMany(projectDependencies: List[ProjectDependency])
        : ConnectionIO[Int] =
      val sql = """
      INSERT INTO projectDependency (
        projectName,
        groupName,
        timestamp,
        dependencyId)
      VALUES (?, ?, ?, ?)
      """
      Update[ProjectDependency](sql).updateMany(projectDependencies)
