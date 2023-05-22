package application.services

import cats.Monad
import cats.data.Validated.{ Invalid, Valid }
import cats.implicits.*
import core.domain.Exporter
import core.domain.delta.ScanDelta
import core.domain.project.{ Project, ScanResultRepository }
import org.joda.time.DateTime
import org.legogroup.woof.{ *, given }

trait ScanDeltaExportService[F[_]]:
  def exportDeltas(
      projects: List[Project],
      leftTimestamp: DateTime,
      rightTimeStamp: DateTime
  ): F[Unit]

object ScanDeltaExportService:
  def make[F[_]: Monad: Logger](
      exporter: Exporter[F, ScanDelta],
      repository: ScanResultRepository[F]
  ): ScanDeltaExportService[F] = new:
    def exportDeltas(
        projects: List[Project],
        leftTimestamp: DateTime,
        rightTimeStamp: DateTime
    ): F[Unit] =
      val names = projects.map(_.name)
      (
        repository.getScanReports(names, leftTimestamp),
        repository.getScanReports(names, rightTimeStamp)
      ).tupled.flatMap((leftScans, rightScans) =>
        val scans = leftScans.map(leftScan =>
          rightScans.find(_.projectName == leftScan.projectName).map(
            rightScan =>
              ScanDelta(leftScan, rightScan)
          )
        ).collect {
          case Some(scan) => scan
        }

        var errors     = List.empty[String]
        var validScans = List.empty[ScanDelta]
        scans.foreach {
          case Valid(scan)      => validScans = scan :: validScans
          case Invalid(reasons) => errors = reasons.mkString_(",") :: errors
        }

        errors.traverse(Logger[F].error)
          *> exporter.exportData(orderScans(projects, validScans))
          *> Logger[F].info(
            s"Successfully exported deltas of ${validScans.length} scans"
          )
      )

  private def orderScans(
      intendedOrder: List[Project],
      scans: List[ScanDelta]
  ): List[ScanDelta] =
    intendedOrder.flatMap(project => scans.find(_.projectName == project.name))
