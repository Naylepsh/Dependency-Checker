package infra.exporters

import cats._
import cats.implicits._
import domain.Exporter

object ConsoleExporter:
  def make[F[_]: Applicative, A]: Exporter[F, A] = new Exporter[F, A]:
    override def exportData(data: List[A]): F[Unit] = println(data).pure
