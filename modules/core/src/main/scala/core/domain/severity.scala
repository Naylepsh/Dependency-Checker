package core.domain

import org.joda.time.DateTime

import dependency.DependencyScanReport

object severity:
  import semver.*

  enum Severity:
    case Unknown, None, Low, Medium, High

  def determineSeverity(now: DateTime, dependency: DependencyScanReport): Severity =
    (
      dependency.vulnerabilities.isEmpty,
      determineSeverityOnVersionDiff(dependency),
      dependency.isMaintained(now)
    ) match
      case (false, _, _)                         => Severity.High
      case (true, Severity.Unknown, Some(false)) => Severity.Medium
      case (true, Severity.None, Some(false))    => Severity.Medium
      case (true, Severity.Low, Some(false))     => Severity.Medium
      case (true, versionSeverity, _)            => versionSeverity

  private def determineSeverityOnVersionDiff(
      dependency: DependencyScanReport
  ): Severity =
    dependency.currentVersion
      .map(current =>
        calculateVersionDifference(current, dependency.latestVersion)
          .map {
            case VersionDifference.Major => Severity.High
            case VersionDifference.Minor => Severity.Medium
            case VersionDifference.Patch => Severity.Low
          }
          .getOrElse(Severity.None)
      )
      .getOrElse(Severity.Unknown)
