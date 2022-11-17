package domain

import domain.dependency.DependencyReport

object severity {
  import semver._

  enum Severity {
    case Unknown, None, Low, Medium, High
  }

  def determineSeverity(dependency: DependencyReport): Severity = {
    if (!dependency.vulnerabilities.isEmpty)
      Severity.High
    else
      determineSeverityOnVersionDiff(dependency)
  }

  private def determineSeverityOnVersionDiff(
      dependency: DependencyReport
  ): Severity = {
    dependency.currentVersion
      .map(current => {
        calculateVersionDifference(current, dependency.latestVersion)
          .map(_ match {
            case VersionDifference.Major => Severity.High
            case VersionDifference.Minor => Severity.Medium
            case VersionDifference.Patch => Severity.Low
          })
          .getOrElse(Severity.None)
      })
      .getOrElse(Severity.Unknown)
  }

}
