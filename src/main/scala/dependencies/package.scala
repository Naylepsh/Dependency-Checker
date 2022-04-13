package object dependencies {
  case class Dependency(
      name: String,
      currentVersion: Option[String],
      latestVersion: Option[String]
  )
}
