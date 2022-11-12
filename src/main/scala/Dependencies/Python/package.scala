package Dependencies

import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext
import Dependencies._

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
      getDependencyFile: String => Future[DependencyFile],
      getDependencyDetails: Dependency => Try[PackageDetails]
  )(path: String)(using ExecutionContext): Future[List[Dependency]] = {
    for {
      file <- getDependencyFile(path)
      dependencies <- Python.getDependencies(file, getDependencyDetails)
    } yield dependencies
  }

  def getDependencies(
      file: DependencyFile,
      getDependencyDetails: Dependency => Try[PackageDetails]
  )(using ExecutionContext): Future[List[Dependency]] = {
    Future.sequence(
      matchParser(file.format)
        .parse(file.content)
        .map(dependency =>
          Future(
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
          )
        )
    )
  }

  private def matchParser(format: DependencyFileFormat): DependencyFormat =
    format match {
      case Txt  => RequirementsTxt
      case Toml => PyProjectToml
    }
}
