package scanning.domain

import core.domain.Grouped
import core.domain.dependency.DependencyScanReport

case class DependencySummary(
    scanReport: DependencyScanReport,
    canUpdate: Boolean
)

case class ScanSummary(
    projectName: String,
    dependencySummaries: List[Grouped[DependencySummary]]
)
