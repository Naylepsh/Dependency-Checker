package application.services

import domain.project.Project
import domain.Exporter
import domain.delta.ScanDelta
import domain.project.ScanResultRepository
import org.joda.time.DateTime
import cats.implicits.*
import cats.Monad
import org.legogroup.woof.{ *, given }
import cats.data.Validated.Valid
import cats.data.Validated.Invalid

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
      )

  private def orderScans(
      intendedOrder: List[Project],
      scans: List[ScanDelta]
  ): List[ScanDelta] =
    intendedOrder.flatMap(project => scans.find(_.projectName == project.name))
