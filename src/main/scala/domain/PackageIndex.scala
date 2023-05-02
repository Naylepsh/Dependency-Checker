package domain

import domain.dependency.DependencyDetails
import domain.dependency.Dependency

trait PackageIndex[F[_]]:
  def getDetails(dependency: Dependency): F[Either[Throwable, DependencyDetails]]
