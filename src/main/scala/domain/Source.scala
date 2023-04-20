package domain

import dependency.*
import project.Grouped

trait Source[F[_], Src]:
  def extract(src: Src): F[List[Grouped[Dependency]]]
