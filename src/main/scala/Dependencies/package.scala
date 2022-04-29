package object Dependencies {
  case class Dependency(
      name: String,
      currentVersion: Option[String],
      latestVersion: Option[String]
  )

  case class RepositoryDependencies(
      name: String,
      dependencies: List[Dependency]
  )
}
