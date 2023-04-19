package application

import cats._
import cats.implicits._
import cats.effect._
import org.legogroup.woof.{given, *}
import scala.concurrent._
import scala.util._
import domain.dependency._
import infra.reporters.python.Pypi

object PythonDependencyReporter:
  def forIo(using Logger[IO]): DependencyReporter[IO] =
    new DependencyReporter[IO]:
      def getDetails(
          dependencies: List[Dependency]
      ): IO[List[DependencyDetails]] =
        dependencies
          .grouped(64)
          .toList
          .parTraverse(
            _.traverse(d => IO.blocking(Pypi.getDependencyDetails(d)))
          )
          .flatMap(results =>
            val (details, exceptions) = results.flatten
              .foldLeft(
                (List.empty[DependencyDetails], List.empty[Throwable])
              ) { case ((results, exceptions), result) =>
                result match
                  case Failure(exception) => (results, exception :: exceptions)
                  case Success(value)     => (value :: results, exceptions)
              }
            exceptions.traverse(exc =>
              Logger[IO].error(exc.toString)
            ) *> details.pure
          )
