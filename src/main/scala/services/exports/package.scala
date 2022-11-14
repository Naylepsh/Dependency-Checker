package services

import domain.dependency.Dependency

package object exports {
  trait Exporter[F[_], A] {
    def exportData(data: List[A]): F[Unit]
  }
}
