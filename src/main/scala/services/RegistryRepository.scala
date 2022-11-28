package services

import cats.effect._
import domain.registry.Project
import utils.json
import domain.registry.Registry
import scala.io.Source

trait RegistryRepository[F[_]]:
  def get(): F[Registry]

object RegistryRepository:
  def fileBased(pathToFile: String): RegistryRepository[IO] =
    new RegistryRepository[IO]:
      def get(): IO[Registry] = IO {
        val content = Source.fromFile(pathToFile).getLines.mkString("\n")
        json.parse[Registry](content)
      }
