package core.domain

import java.util.UUID

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.*
import org.joda.time.Days

import vulnerability.*

object dependency:
  case class Dependency(
      name: String,
      currentVersion: Option[String]
  )

  case class DependencyLatestRelease(
      name: String,
      version: String,
      releaseDate: DateTime
  )

  case class DependencyDetails(
      name: String,
      ofVersion: String,
      releaseDate: DateTime,
      latestVersion: String,
      latestReleaseDate: Option[DateTime],
      vulnerabilities: List[String] = List(),
      minLanguageVersion: Option[String] = None
  )

  case class DependencyReport(
      name: String,
      currentVersion: Option[String],
      latestVersion: String,
      currentVersionReleaseDate: Option[DateTime],
      latestReleaseDate: Option[DateTime],
      vulnerabilities: List[String] = List(),
      notes: Option[String] = None
  ):
    /**
     * Naively assume that any package that had at least one release
     * within the last 3 years is still maintained
     */
    def isMaintained(now: DateTime): Option[Boolean] = latestReleaseDate.map(
      date =>
        Days.daysBetween(
          date.toLocalDate(),
          now.toLocalDate()
        ).getDays() < 3 * 365
    )
  object DependencyReport:
    def apply(
        dependency: Dependency,
        details: DependencyDetails,
        notes: Option[String]
    ): DependencyReport = DependencyReport(
      dependency.name,
      dependency.currentVersion,
      details.latestVersion,
      Some(details.releaseDate),
      details.latestReleaseDate,
      details.vulnerabilities,
      notes
    )

  case class DependencyVulnerability(
      name: String,
      severity: Option[VulnerabilitySeverity]
  )

  case class DependencyScanReport(
      name: String,
      currentVersion: Option[String],
      latestVersion: String,
      currentVersionReleaseDate: Option[DateTime],
      latestReleaseDate: Option[DateTime],
      vulnerabilities: List[DependencyVulnerability] = List()
  ):
    /**
     * Naively assume that any package that had at least one release
     * within the last 3 years is still maintained
     */
    def isMaintained(now: DateTime): Option[Boolean] = latestReleaseDate.map:
      date =>
        Days.daysBetween(
          date.toLocalDate(),
          now.toLocalDate()
        ).getDays() < 3 * 365

  object DependencyScanReport:
    def compareByNameAsc(
        a: DependencyScanReport,
        b: DependencyScanReport
    ): Int =
      a.name.toLowerCase.compare(b.name.toLowerCase).sign

    def compareByNameDesc(
        a: DependencyScanReport,
        b: DependencyScanReport
    ): Int =
      b.name.toLowerCase.compare(a.name.toLowerCase).sign

    def compareBySeverityAsc(now: DateTime)(
        a: DependencyScanReport,
        b: DependencyScanReport
    ): Int =
      val sa = severity.determineSeverity(now, a)
      val sb = severity.determineSeverity(now, b)
      sa.ordinal.compare(sb.ordinal).sign

    def compareBySeverityDesc(now: DateTime)(
        a: DependencyScanReport,
        b: DependencyScanReport
    ): Int =
      val sa = severity.determineSeverity(now, a)
      val sb = severity.determineSeverity(now, b)
      sb.ordinal.compare(sa.ordinal).sign

  sealed trait DependencySource:
    val path: String
    val groupName: String
  object DependencySource:
    case class TxtSource(path: String) extends DependencySource:
      val groupName: String = path

    case class TomlSource(path: String, group: Option[String] = None)
        extends DependencySource:
      val groupName: String = group.fold(path)(g => s"$path:$g")

  trait DependencyScanner[F[_]]:
    def getDetails(dependencies: List[Dependency]): F[List[DependencyDetails]]

  case class ExistingDependency(id: UUID, name: String)

  trait DependencyRepository[F[_]]:
    def save(
        dependencies: List[DependencyReport],
        timestamp: DateTime
    ): F[List[ExistingDependency]]
    def delete(timestamps: NonEmptyList[DateTime]): F[Unit]
    def findLatestReleases(ids: List[UUID]): F[List[DependencyLatestRelease]]
