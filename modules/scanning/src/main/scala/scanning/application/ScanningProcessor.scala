package scanning.application

import cats.Monad
import cats.syntax.all.*
import core.domain.dependency.DependencyScanReport
import core.domain.project.ProjectScanConfigRepository
import core.domain.task.TaskProcessor
import core.domain.update.{ UpdateDependency, UpdateGateway }
import scanning.application.services.ScanningService

trait ScanningProcessor[F[_]]:
  def scanAll: F[Unit]
  def scanSingle(projectName: String): F[Option[Unit]]

object ScanningProcessor:
  def make[F[_]: Monad](
      service: ScanningService[F],
      repository: ProjectScanConfigRepository[F],
      taskProcessor: TaskProcessor[F],
      updateGateway: UpdateGateway[F]
  ): ScanningProcessor[F] = new:

    def scanSingle(projectName: String): F[Option[Unit]] =
      repository.all.flatMap: configs =>
        configs
          .find: config =>
            config.project.name == projectName
          .traverse: config =>
            taskProcessor.add(service.scan(config.toProjectScanConfig))

    def scanAll: F[Unit] =
      repository
        .all
        .flatMap: configs =>
          val enabledConfigs = configs.filter(_.enabled)

          val scan = enabledConfigs.traverse: config =>
            taskProcessor.add(service.scan(config.toProjectScanConfig))
          val checkVulnerabilities =
            taskProcessor.add(service.obtainUnknownSeveritiesOfVulnerabilities)
          val updateDependencies = enabledConfigs
            .traverse: config =>
              // TODO: Check whether automatic updates are enabled
              service.getLatestScan(
                config.project.name,
                DependencyScanReport.compareByNameAsc
              )
            .map: scans =>
              scans.collect:
                case Some(scan) => scan
            .flatMap: scans =>
              var depsToUpdate = List.empty[UpdateDependency]
              scans.foreach: scan =>
                scan.dependencySummaries.foreach: summary =>
                  summary.items.foreach: dependencySummary =>
                    dependencySummary.scanReport.currentVersion.foreach:
                      currentVersion =>
                        depsToUpdate = UpdateDependency(
                          scan.projectName,
                          dependencySummary.scanReport.name,
                          summary.groupName,
                          currentVersion,
                          dependencySummary.scanReport.latestVersion
                        ) :: depsToUpdate
              taskProcessor.add(updateGateway.update(depsToUpdate).void)

          scan *> checkVulnerabilities *> updateDependencies
