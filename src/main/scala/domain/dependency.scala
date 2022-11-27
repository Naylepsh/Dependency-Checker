package domain

object dependency {
  case class Dependency(
      name: String,
      currentVersion: Option[String]
  )

  case class DependencyDetails(
      name: String,
      ofVersion: String,
      latestVersion: String,
      vulnerabilities: List[String] = List(),
      minLanguageVersion: Option[String] = None
  )

  case class DependencyReport(
      name: String,
      currentVersion: Option[String],
      latestVersion: String,
      vulnerabilities: List[String] = List(),
      notes: Option[String] = None
  )
  object DependencyReport {
    def apply(
        dependency: Dependency,
        details: DependencyDetails,
        notes: Option[String]
    ): DependencyReport = DependencyReport(
      dependency.name,
      dependency.currentVersion,
      details.latestVersion,
      details.vulnerabilities,
      notes
    )
  }

}
