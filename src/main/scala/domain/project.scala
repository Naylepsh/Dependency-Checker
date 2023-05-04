package domain

import domain.dependency.*
import org.joda.time.DateTime

object project:
  case class Project(id: String, name: String)

  case class Grouped[A](
      groupName: String,
      items: List[A]
  )

  case class ProjectDependencies(
      project: Project,
      dependencies: List[Grouped[Dependency]]
  )

  case class ScanResult(
      project: Project,
      dependenciesReports: List[Grouped[DependencyReport]]
  )

  trait ScanResultRepository[F[_]]:
    def save(results: List[ScanResult], timestamp: DateTime): F[Unit]
