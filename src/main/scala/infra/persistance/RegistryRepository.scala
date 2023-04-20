package infra.persistance

import scala.io.Source

import cats.effect.*
import domain.registry.{Project, Registry, RegistryRepository}
import infra.json

object RegistryRepository:
  def fileBased(pathToFile: String): RegistryRepository[IO] =
    new RegistryRepository[IO]:
      def get(): IO[Registry] = IO {
        val content = Source.fromFile(pathToFile).getLines.mkString("\n")
        json.parse[Registry](content)
      }
