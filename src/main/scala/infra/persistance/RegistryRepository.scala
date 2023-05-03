package infra.persistance

import scala.io.Source

import cats.implicits.*
import cats.effect.*
import domain.registry.{ Project, Registry, RegistryRepository }
import io.circe.parser.decode

object RegistryRepository:
  def fileBased(pathToFile: String): RegistryRepository[IO] =
    new RegistryRepository[IO]:
      def get(): IO[Either[Throwable, Registry]] = IO {
        val content = Source.fromFile(pathToFile).getLines.mkString("\n")
        decode[Registry](content).leftMap(_.getCause)
      }
