package domain

import cats.data.ValidatedNel
import cats.implicits.*
import domain.project.ScanReport

object delta:
  import domain.dependency.DependencyReport

  sealed trait PropertyDelta
  object PropertyDelta:
    case class CurrentVersion(left: Option[String], right: Option[String])
        extends PropertyDelta
    case class LatestVersion(left: String, right: String) extends PropertyDelta
    case class VulnerabilityCount(left: Int, right: Int)  extends PropertyDelta

  case class TotalDelta(
      currentVersion: Option[PropertyDelta.CurrentVersion],
      latestVersion: Option[PropertyDelta.LatestVersion],
      vulnerabilityCount: Option[PropertyDelta.VulnerabilityCount]
  ):
    val isEmpty =
      currentVersion.isEmpty
        && latestVersion.isEmpty
        && vulnerabilityCount.isEmpty

  object TotalDelta:
    private[delta] def apply(
        left: DependencyReport,
        right: DependencyReport
    ): TotalDelta =
      val currentVersion = (left.currentVersion, right.currentVersion) match
        case (Some(x), Some(y)) if x != y =>
          PropertyDelta.CurrentVersion(
            left.currentVersion,
            right.currentVersion
          ).some
        case (None, Some(_)) | (Some(_), None) =>
          PropertyDelta.CurrentVersion(
            left.currentVersion,
            right.currentVersion
          ).some
        case _ => None

      val latestVersion = if left.latestVersion != right.latestVersion then
        PropertyDelta.LatestVersion(
          left.latestVersion,
          right.latestVersion
        ).some
      else None

      val leftVulnCount  = left.vulnerabilities.length
      val rightVulnCount = right.vulnerabilities.length
      val vulnerabilityCount = if leftVulnCount != rightVulnCount then
        PropertyDelta.VulnerabilityCount(
          leftVulnCount,
          rightVulnCount
        ).some
      else None

      TotalDelta(currentVersion, latestVersion, vulnerabilityCount)

  case class DependencyDelta(
      name: String,
      delta: TotalDelta
  )
  object DependencyDelta:
    def apply(
        left: DependencyReport,
        right: DependencyReport
    ): Either[String, DependencyDelta] =
      Either.cond(
        left.name == right.name,
        DependencyDelta(left.name, TotalDelta(left, right)),
        s"Left (${left.name}) and Right (${right.name}) are different dependencies"
      )

  case class ScanDelta(
      projectName: String,
      dependenciesDeltas: List[Grouped[DependencyDelta]]
  )
  object ScanDelta:
    def apply(
        left: ScanReport,
        right: ScanReport
    ): ValidatedNel[String, ScanDelta] =
      (
        validateProjectName(left, right),
        validateDependencyDeltas(left, right)
      ).mapN(ScanDelta(_, _))

    private def validateProjectName(
        left: ScanReport,
        right: ScanReport
    ): ValidatedNel[String, String] =
      Either.cond(
        left.projectName == right.projectName,
        left.projectName,
        s"Left (${left.projectName}) and Right (${right.projectName}) are different projects"
      ).toValidatedNel

    /**
     * Assumes that left and right represent the same scan, but at a different time
     */
    private def validateDependencyDeltas(
        left: ScanReport,
        right: ScanReport
    ): ValidatedNel[String, List[Grouped[DependencyDelta]]] =
      left
        .dependenciesReports
        .traverse(group =>
          right
            .dependenciesReports
            .find(_.groupName == group.groupName)
            .map(rightGroup => validateGroupDeltas(group, rightGroup))
            .getOrElse(List.empty.validNel[String])
            .map(deltas => Grouped(group.groupName, deltas))
        )

    /**
     * Assumes that left and right represent the same group, but at a different time
     */
    private def validateGroupDeltas(
        left: Grouped[DependencyReport],
        right: Grouped[DependencyReport]
    ): ValidatedNel[String, List[DependencyDelta]] =
      left
        .items
        .traverse(report =>
          right
            .items
            .find(_.name == report.name)
            .map(DependencyDelta(report, _).toValidatedNel)
        )
        .getOrElse(List.empty)
        .foldLeft(List.empty[DependencyDelta].validNel[String])((acc, delta) =>
          acc.combine(delta.map(List(_)))
        )
