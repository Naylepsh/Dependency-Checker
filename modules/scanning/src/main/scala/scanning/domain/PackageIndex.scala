package scanning.domain

import core.domain.dependency.{ Dependency, DependencyDetails }

trait PackageIndex[F[_]]:
  def getDetails(dependency: Dependency): F[Either[String, DependencyDetails]]
