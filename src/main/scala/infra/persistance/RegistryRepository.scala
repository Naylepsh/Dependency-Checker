package infra.persistance

import cats.effect._
import domain.registry.Project
import domain.registry.Registry
import domain.registry.RegistryRepository
import infra.json
import scala.io.Source

object RegistryRepository:
  def fileBased(pathToFile: String): RegistryRepository[IO] =
    new RegistryRepository[IO]:
      def get(): IO[Registry] = IO {
        val content = Source.fromFile(pathToFile).getLines.mkString("\n")
        json.parse[Registry](content)
      }
