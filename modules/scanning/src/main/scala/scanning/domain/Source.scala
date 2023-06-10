package scanning.domain

import core.domain.dependency.Dependency
import core.domain.Grouped

trait Source[F[_], Src]:
  def extract(src: Src): F[List[Grouped[Dependency]]]
