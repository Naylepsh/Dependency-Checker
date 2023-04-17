package services

import domain.dependency._
import domain.project.Grouped

package object sources {
  trait Source[F[_], Src] {
    def extract(src: Src): F[List[Grouped[Dependency]]]
  }
}
