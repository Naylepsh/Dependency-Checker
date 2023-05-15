package domain

import domain.project.ScanReport
import cats.data.ValidatedNel
import cats.implicits.*

object delta:
  import domain.dependency.DependencyReport

  case class DependencyShort(
      currentVersion: Option[String],
      latestVersion: String,
      vulnerabilityCount: Int
  )
  object DependencyShort:
    def apply(report: DependencyReport): DependencyShort =
      DependencyShort(
        report.currentVersion,
        report.latestVersion,
        report.vulnerabilities.length
      )

  case class DependencyDelta(
      name: String,
      left: DependencyShort,
      right: DependencyShort
  )
  object DependencyDelta:
    def apply(
        left: DependencyReport,
        right: DependencyReport
    ): Either[String, DependencyDelta] =
      Either.cond(
        left.name == right.name,
        DependencyDelta(
          left.name,
          DependencyShort(left),
          DependencyShort(right)
        ),
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
