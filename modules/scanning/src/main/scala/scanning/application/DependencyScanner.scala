package scanning.application

import cats.Monad
import cats.syntax.all.*
import org.legogroup.woof.{ *, given }
import scanning.domain.PackageIndex
import core.domain.dependency.*

object DependencyScanner:
  def make[F[_]: Monad: Logger](packageIndex: PackageIndex[F])
      : DependencyScanner[F] = new:
    def getDetails(dependencies: List[Dependency]): F[List[DependencyDetails]] =
      dependencies
        .traverse(packageIndex.getDetails)
        .map(aggregateResults)
        .flatMap: (errors, details) =>
          errors.traverse(Logger[F].error) *> details.pure

  private def aggregateResults(results: List[Either[String, DependencyDetails]])
      : (List[String], List[DependencyDetails]) =
    results.foldLeft((List.empty[String], List.empty[DependencyDetails])):
      case ((errors, details), result) =>
        result match
          case Left(error)   => (error :: errors, details)
          case Right(result) => (errors, result :: details)
