package parsers.python

import java.nio.file.{ Files, Paths, StandardOpenOption }
import java.util.UUID

import scala.sys.process.*

import cats.effect.kernel.Sync
import cats.syntax.all.*

case class PoetryFiles(pyProjectContent: String, lockContent: String)

trait Poetry[F[_]]:
  def update(
      packageName: String,
      from: String,
      to: String,
      files: PoetryFiles
  ): F[Either[Throwable, PoetryFiles]]

object Poetry:
  def make[F[_]: Sync]: Poetry[F] = new:
    def update(
        packageName: String,
        from: String,
        to: String,
        files: PoetryFiles
    ): F[Either[Throwable, PoetryFiles]] =
      val newPyProjectContent =
        updatePackage(packageName, from, to, files.pyProjectContent)
      val newFiles = files.copy(pyProjectContent = newPyProjectContent)
      updateLock(newFiles).map: result =>
        result.map: newLockContent =>
          newFiles.copy(lockContent = newLockContent)

    private def updateLock(files: PoetryFiles): F[Either[Throwable, String]] =
      val dir           = Paths.get(s"./data/poetry/${UUID.randomUUID()}")
      val pyProjectPath = dir.resolve(Paths.get("pyproject.toml"))
      val lockPath      = dir.resolve(Paths.get("poetry.lock"))

      Sync[F]
        .delay:
          Files.createDirectories(dir)
          Files.write(
            pyProjectPath,
            files.pyProjectContent.getBytes,
            StandardOpenOption.CREATE
          )
          Files.write(
            lockPath,
            files.lockContent.getBytes,
            StandardOpenOption.CREATE
          )

          s"poetry lock --directory=$dir".!!

          Files.readString(lockPath)
        .attempt
        .flatTap: _ =>
          Sync[F].delay:
            Files.deleteIfExists(pyProjectPath)
            Files.deleteIfExists(lockPath)

  def updatePackage(
      packageName: String,
      from: String,
      to: String,
      pyProjectContent: String
  ): String =
    pyProjectContent
      .split("\n")
      .map: line =>
        val index                = line.indexOf(packageName)
        val indexOfCharAfterName = index + packageName.length
        val isLineNameAndVersion = index == 0
          && line.length > indexOfCharAfterName
          && line(indexOfCharAfterName) == ' '
        if isLineNameAndVersion then
          line.replace(from, to)
        else
          line
      .mkString("\n") + "\n"
