package domain

import domain.dependency._

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

  case class ExportProjectDependencies(
      project: Project,
      dependenciesReports: List[Grouped[DependencyReport]]
  )
