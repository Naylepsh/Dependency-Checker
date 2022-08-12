package Dependencies

import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext

package object Python {
  case class PackageDetails(
      latestVersion: Option[String],
      vulnerabilities: List[String],
      requiredPython: Option[String]
  ) {
    // Things that are generally unknown when just parsing the local file
  }

  trait DependencyFormat {
    def parse(fileContents: String): List[Dependency]
  }

  def getDependencies(
      getFileContents: String => Future[String],
      getDependencyDetails: Dependency => Try[PackageDetails]
  )(path: String)(implicit ec: ExecutionContext): Future[List[Dependency]] = {
    for {
      fileContents <- getFileContents(path)
      dependencies <- Python.getDependencies(fileContents, getDependencyDetails)
    } yield dependencies
  }

  def getDependencies(
      fileContents: String,
      getDependencyDetails: Dependency => Try[PackageDetails]
  )(implicit ec: ExecutionContext): Future[List[Dependency]] = {
    // TODO: Infer parser
    val dependenciesFutures = RequirementsTxt
      .parse(fileContents)
      .map(dependency =>
        Future {
          getDependencyDetails(dependency)
            .map(details => {
              dependency.copy(
                latestVersion = details.latestVersion,
                vulnerabilities = details.vulnerabilities,
                notes = details.requiredPython
                  .map(version => s"Required python: ${version}")
              )
            })
            .getOrElse(dependency)
        }
      )
    Future.sequence(dependenciesFutures)
  }
}
