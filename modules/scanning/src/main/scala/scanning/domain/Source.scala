package scanning.domain

import core.domain.Grouped
import core.domain.dependency.Dependency

trait Source[F[_], Src]:
  def extract(src: Src): F[List[Grouped[Dependency]]]
