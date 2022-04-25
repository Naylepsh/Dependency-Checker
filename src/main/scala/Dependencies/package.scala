package object Dependencies {
  case class Dependency(
      name: String,
      currentVersion: Option[String],
      latestVersion: Option[String]
  )
}
