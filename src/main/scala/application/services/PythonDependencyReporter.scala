package application.services

import scala.concurrent.*
import scala.util.*

import cats.*
import cats.implicits.*
import domain.PackageIndex
import domain.dependency.*
import org.legogroup.woof.{ *, given }

object PythonDependencyReporter:
  def make[F[_]: Logger: Parallel: Monad](
      packageIndex: PackageIndex[F]
  ): DependencyReporter[F] =
    new DependencyReporter[F]:
      def getDetails(
          dependencies: List[Dependency]
      ): F[List[DependencyDetails]] =
        dependencies
          .grouped(10)
          .toList
          .zipWithIndex
          .traverse(getDetailsOfBatch)
          .flatMap(combineBatchesResults)

      private def getDetailsOfBatch(
          dependencies: List[Dependency],
          batchIndex: Int
      ): F[List[Either[String, DependencyDetails]]] =
        Logger[F].debug(s"Requesting details of $batchIndex-th batch")
          .flatMap(_ =>
            dependencies
              .parTraverse(d =>
                Logger[F]
                  .debug(s"Requesting details of ${d.name}:${d.currentVersion}")
                  .flatMap(_ => packageIndex.getDetails(d))
              ).flatTap(_ =>
                Logger[F].debug(s"Got results for $batchIndex-th batch")
              )
          )

      private def combineBatchesResults(
          batchesResults: List[List[Either[String, DependencyDetails]]]
      ): F[List[DependencyDetails]] =
        val (details, exceptions) = batchesResults.flatten
          .foldLeft(
            (List.empty[DependencyDetails], List.empty[String])
          ) {
            case ((results, exceptions), result) =>
              result match
                case Left(exception) => (results, exception :: exceptions)
                case Right(value)    => (value :: results, exceptions)
          }
        exceptions.traverse(Logger[F].error) *> details.pure
