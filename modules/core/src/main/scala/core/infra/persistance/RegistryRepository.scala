package core.infra.persistance

import scala.io.Source

import cats.Applicative
import cats.effect.*
import cats.implicits.*
import core.domain.registry.{ ProjectScanConfig, Registry, RegistryRepository }
import io.circe.parser.decode

object RegistryRepository:
  def fileBased(pathToFile: String): RegistryRepository[IO] = new:
    def get(): IO[Either[Throwable, Registry]] = IO {
      val content = Source.fromFile(pathToFile).getLines.mkString("\n")
      val decoded = decode[Registry](content)
      decoded.leftMap(error => RuntimeException(error.getMessage()))
    }
