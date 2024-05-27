package scanning.domain

import core.domain.Grouped
import core.domain.dependency.DependencyScanReport

case class DependencySummary(
    scanReport: DependencyScanReport,
    canBeUpdated: Boolean
):
  val shouldBeUpdated: Boolean =
    canBeUpdated && scanReport.versionDifference.isDefined

case class ScanSummary(
    projectName: String,
    dependencySummaries: List[Grouped[DependencySummary]]
)
