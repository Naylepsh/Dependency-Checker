package services.reporters.python

import cats._
import cats.implicits._
import scala.concurrent._
import scala.util._
import domain.dependency._
import services.reporters.DependencyReporter

object PythonDependencyReporter {

  def forFuture(using ExecutionContext) = new DependencyReporter[Future] {
    override def getDetails(
        dependencies: List[Dependency]
    ): Future[List[DependencyDetails]] = {
      dependencies
        .map(d => Future(Pypi.getDependencyDetails(d)))
        .sequence
        .map(
          _.foldLeft(List.empty)((acc, result) =>
            result match
              case Failure(exception) => {
                println(exception)
                acc
              }
              case Success(value) => value :: acc
          )
        )
    }

  }

}
