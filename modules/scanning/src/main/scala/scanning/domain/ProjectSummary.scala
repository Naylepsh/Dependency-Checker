package scanning.domain

import core.domain.project.ProjectScanConfig
import core.domain.project.VulnerabilitySummary
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.Applicative

case class ProjectSummary(
    config: ProjectScanConfig,
    vulnerabilityCount: Int
)
object ProjectSummary:
  def fromScansAndVulnerabilities(
      configs: List[ProjectScanConfig],
      summaries: List[VulnerabilitySummary]
  ): List[ProjectSummary] =
    configs.map: config =>
      val vulnCount = summaries
        .find: summary =>
          summary.projectName == config.project.name
        .map: summary =>
          summary.vulnerabilityCount
        .getOrElse(0)
      ProjectSummary(config, vulnCount)
