package infra.exporters

import cats.*
import cats.implicits.*
import domain.Exporter

object ConsoleExporter:
  def make[F[_]: Applicative, A]: Exporter[F, A] = new:
    override def exportData(data: List[A]): F[Unit] = println(data).pure
