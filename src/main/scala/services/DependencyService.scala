package services

import cats._
import cats.implicits._
import services.sources._
import services.exports._
import services.reporters._
import domain.dependency._
import domain.project._
import scala.annotation.tailrec
import services.sources.GitlabSource.ProjectProps

trait DependencyService[F[_]] {
  def checkDependencies(projects: List[Project]): F[Unit]
}

object DependencyService {
  def make[F[_]: Monad, A](
      source: Source[F, A],
      prepareForSource: Project => Option[A],
      reporter: DependencyReporter[F],
      exporter: Exporter[F, ExportProjectDependencies]
  ): DependencyService[F] = new DependencyService[F] {

    override def checkDependencies(projects: List[Project]): F[Unit] = {
      println(s"Checking dependencies of ${projects.length} projects...")
      for {
        projectsDependencies <- projects
          .traverse { case project @ Project(_, _) =>
            prepareForSource(project)
              .map(source.extract(_))
              .getOrElse(Monad[F].pure(List.empty))
              .map(dependencies => ProjectDependencies(project, dependencies))
          }
        dependencies = projectsDependencies.map(_.dependencies).flatten
        _ = println(s"Checking the details of ${dependencies.length} dependencies...")
        details <- reporter.getDetails(dependencies)
        _ = println("Building the report...")
        detailsMap = details.map(detail => detail.name -> detail).toMap
        reports = projectsDependencies.map(buildReport(detailsMap))
        _ <- exporter.exportData(reports)
      } yield ()
    }
  }

  private def buildReport(details: Map[String, DependencyDetails])(
      projectDependencies: ProjectDependencies
  ): ExportProjectDependencies = {
    @tailrec
    def inner(
        dependencies: List[Dependency],
        report: List[DependencyReport]
    ): List[DependencyReport] = {
      dependencies match
        case head :: next =>
          details.get(head.name) match
            case Some(detail) =>
              inner(next, DependencyReport(head, detail, None) :: report)

            case None => inner(next, report)

        case Nil => report
    }

    val report = inner(projectDependencies.dependencies, List())
    ExportProjectDependencies(projectDependencies.project, report)
  }

}
