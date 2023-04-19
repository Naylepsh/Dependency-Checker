package domain

trait Exporter[F[_], A]:
  def exportData(data: List[A]): F[Unit]
