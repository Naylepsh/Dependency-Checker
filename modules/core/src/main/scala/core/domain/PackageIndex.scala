package core.domain

import dependency.{ Dependency, DependencyDetails }

trait PackageIndex[F[_]]:
  def getDetails(dependency: Dependency): F[Either[String, DependencyDetails]]
