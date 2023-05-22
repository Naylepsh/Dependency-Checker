package core.domain

import dependency.*

trait Source[F[_], Src]:
  def extract(src: Src): F[List[Grouped[Dependency]]]
