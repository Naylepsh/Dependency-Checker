package scanning.application

import core.domain.project.ProjectScanConfig
import scanning.domain.ProjectSummary
import core.domain.project.ScanResultRepository
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.Applicative

trait ProjectSummaryService[F[_]]:
  def enrichWithScanSummary(configs: List[ProjectScanConfig])
      : F[List[ProjectSummary]]
  def enrichWithScanSummary(config: ProjectScanConfig): F[ProjectSummary]

object ProjectSummaryService:
  def make[F[_]: Applicative](repository: ScanResultRepository[F])
      : ProjectSummaryService[F] = new:
    def enrichWithScanSummary(configs: List[ProjectScanConfig])
        : F[List[ProjectSummary]] =
      NonEmptyList
        .fromList:
          configs.map: config =>
            config.project.name
        .map: names =>
          repository
            .getVulnerabilitySummary(names)
            .map: summaries =>
              ProjectSummary.fromScansAndVulnerabilities(configs, summaries)
        .getOrElse(List.empty.pure)

    def enrichWithScanSummary(config: ProjectScanConfig): F[ProjectSummary] =
      repository
        .getVulnerabilitySummary(NonEmptyList.of(config.project.name))
        .map: summaries =>
          ProjectSummary
            .fromScansAndVulnerabilities(List(config), summaries)
            .head
