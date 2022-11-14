package services

import domain.dependency._

package object sources {
  trait Source[F[_], Src] {
    def extract(src: Src): F[List[Dependency]]
  }
}
