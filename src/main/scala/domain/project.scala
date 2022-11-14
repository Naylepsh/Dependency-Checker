package domain

import domain.dependency._

object project {
  case class Project(id: String, name: String)
  case class ProjectDependencies(
      project: Project,
      dependencies: List[Dependency]
  )

  case class ExportProjectDependencies(
      project: Project,
      dependenciesReports: List[DependencyReport]
  )
}
