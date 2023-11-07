package scanning.domain

import core.domain.Grouped
import core.domain.dependency.DependencyScanReport

case class DependencySummary(
    scanReport: DependencyScanReport,
    canBeUpdated: Boolean
)

case class ScanSummary(
    projectName: String,
    dependencySummaries: List[Grouped[DependencySummary]]
)
