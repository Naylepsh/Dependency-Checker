package core.application

import org.http4s.HttpRoutes

object controller:
  trait Controller[F[_]]:
    def routes: HttpRoutes[F]
