package Dependencies

import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.concurrent.Future
import Dependencies.Utils.tryToOption

package object Python {
  trait DependencyFormat {
    def parse(fileContents: String): List[Dependency]
  }

  def getDependencies(
      getFileContents: String => Future[String],
      getLatestVersion: String => Try[String]
  )(path: String)(using ExecutionContext): Future[List[Dependency]] = {
    for {
      fileContents <- getFileContents(path)
      dependencies <- getDependencies(fileContents, getLatestVersion)
    } yield dependencies
  }

  def getDependencies(
      fileContents: String,
      getLatestVersion: String => Try[String]
  )(using ExecutionContext): Future[List[Dependency]] = {
    val dependenciesFutures = Python.RequirementsTxt
      .parse(fileContents)
      .map(dependency =>
        Future {
          dependency.copy(latestVersion =
            tryToOption(getLatestVersion(dependency.name))
          )
        }
      )
    Future.sequence(dependenciesFutures)
  }
}
