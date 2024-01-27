package parsers.python

import cats.syntax.all.*

enum RequirementsUpdateError:
  case FailedToChangeContent

trait Requirements:
  def update(
      packageName: String,
      from: String,
      to: String,
      file: PackageManagementFiles.RequirementFile
  ): Either[RequirementsUpdateError, PackageManagementFiles.RequirementFile]

object Requirements:
  def make: Requirements = new:
    def update(
        packageName: String,
        from: String,
        to: String,
        file: PackageManagementFiles.RequirementFile
    ): Either[RequirementsUpdateError, PackageManagementFiles.RequirementFile] =
      val simplePattern = (s"^(" + packageName + ")([<>=!~^]+)(.*)$").r
      val withExtraPattern =
        (s"^(" + packageName + ")\\[(.*)\\]([<>=!~^]+)(.*)$").r

      val updatedFileContent = file.content
        .split("\n")
        .map:
          case simplePattern(name, operator, version)
              if version == from => s"$name$operator$to"
          case withExtraPattern(name, extra, operator, version)
              if version == from => s"$name[$extra]$operator$to"
          case line => line
        .mkString("\n") + "\n"

      if updatedFileContent == file.content
      then RequirementsUpdateError.FailedToChangeContent.asLeft
      else PackageManagementFiles.RequirementFile(updatedFileContent).asRight
