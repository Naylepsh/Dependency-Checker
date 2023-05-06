package domain

import java.util.UUID

import com.github.nscala_time.time.Imports.*
import org.joda.time.Days

object dependency:
  case class Dependency(
      name: String,
      currentVersion: Option[String]
  )

  case class DependencyDetails(
      name: String,
      ofVersion: String,
      latestVersion: String,
      latestReleaseDate: Option[DateTime],
      vulnerabilities: List[String] = List(),
      minLanguageVersion: Option[String] = None
  )

  case class DependencyReport(
      name: String,
      currentVersion: Option[String],
      latestVersion: String,
      latestReleaseDate: Option[DateTime],
      vulnerabilities: List[String] = List(),
      notes: Option[String] = None
  ):
    // Naively assume that any package at least one release within the last year is still maintained
    def isMaintained(now: DateTime): Option[Boolean] = latestReleaseDate.map(
      date =>
        Days.daysBetween(
          date.toLocalDate(),
          now.toLocalDate()
        ).getDays() < 365
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
      details.latestReleaseDate,
      details.vulnerabilities,
      notes
    )

  trait DependencyReporter[F[_]]:
    def getDetails(dependencies: List[Dependency]): F[List[DependencyDetails]]

  case class ExistingDependency(
      id: UUID,
      timestamp: DateTime,
      name: String
  )

  trait DependencyRepository[F[_]]:
    def save(
        dependencies: List[DependencyReport],
        timestamp: DateTime
    ): F[List[ExistingDependency]]
