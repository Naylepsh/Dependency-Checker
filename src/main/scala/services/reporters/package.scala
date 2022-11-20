package services

import domain.dependency._

package object reporters {
  trait DependencyReporter[F[_]] {
    def getDetails(dependencies: List[Dependency]): F[List[DependencyDetails]]
  }
}
