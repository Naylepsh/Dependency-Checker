package application

import scala.concurrent.*
import scala.util.*

import cats.*
import cats.effect.*
import cats.implicits.*
import domain.dependency.*
import org.legogroup.woof.{ *, given }
import domain.PackageIndex

object PythonDependencyReporter:
  def forIo(packageIndex: PackageIndex[IO])(using
  Logger[IO]): DependencyReporter[IO] =
    new DependencyReporter[IO]:
      def getDetails(
          dependencies: List[Dependency]
      ): IO[List[DependencyDetails]] =
        dependencies
          .grouped(64)
          .toList
          .parTraverse(
            _.traverse(d => packageIndex.getDetails(d))
          )
          .flatMap(results =>
            val (details, exceptions) = results.flatten
              .foldLeft(
                (List.empty[DependencyDetails], List.empty[Throwable])
              ) {
                case ((results, exceptions), result) =>
                  result match
                    case Left(exception) => (results, exception :: exceptions)
                    case Right(value)    => (value :: results, exceptions)
              }
            exceptions.traverse(exc =>
              Logger[IO].error(exc.toString)
            ) *> details.pure
          )
