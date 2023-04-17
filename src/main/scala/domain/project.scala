package domain

import domain.dependency._

object project:
  case class Project(id: String, name: String)

  case class GroupedDependencies(
    groupName: String,
    dependencies: List[Dependency]
  )

  case class ProjectDependencies(
      project: Project,
      dependencies: List[GroupedDependencies]
  )

  case class ExportProjectDependencies(
      project: Project,
      dependenciesReports: List[DependencyReport]
  )
