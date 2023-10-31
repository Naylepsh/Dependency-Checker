package update

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import core.domain.project.ProjectScanConfigRepository
import gitlab.{ Action, CommitAction, GitlabApi }

import domain.{
  UpdateAttempt,
  UpdateDependency,
  UpdateDependencyDetails,
  UpdateRepository,
  UpdateService
}

object services:
  object UpdateService:
    def make[F[_]: Monad](
        repository: UpdateRepository[F],
        projectConfigRepository: ProjectScanConfigRepository[F],
        gitlabApi: GitlabApi[F]
    ): UpdateService[F] = new:

      def update(request: UpdateDependency): F[Either[String, Unit]] =
        projectConfigRepository
          .findByProjectName(request.projectName)
          .flatMap:
            case None => "Config for project not found".asLeft.pure
            case Some(config) =>
              val req = UpdateDependencyDetails(
                projectId = config.id,
                projectBranch = config.branch,
                projectGitlabId = config.project.repositoryId,
                filePath = request.filePath,
                dependencyName = request.dependencyName,
                fromVersion = request.fromVersion,
                toVersion = request.toVersion
              )
              update(req)

      def update(request: UpdateDependencyDetails): F[Either[String, Unit]] =
        FileType.fromPath(request.filePath) match
          case Left(reason) => reason.asLeft.pure
          case Right(fileType) =>
            EitherT(ensureAttemptWasNotMadeBefore(request))
              .flatMap: _ =>
                EitherT(updateDependencyFile(request, fileType))
              .flatMap: updatedContent =>
                EitherT(publishToGit(request, updatedContent))
              .flatTap: mergeRequest =>
                val attempt = UpdateAttempt(
                  request.projectId,
                  request.dependencyName,
                  request.toVersion,
                  mergeRequest.webUrl
                )
                EitherT(repository.save(attempt).map(_.asRight))
              .void
              .value

      private def updateDependencyFile(
          request: UpdateDependencyDetails,
          fileType: FileType
      ): F[Either[String, String]] =
        getFileContent(
          request.projectGitlabId,
          request.projectBranch,
          request.filePath
        )
          .map:
            case Left(error) => error.pure
            case Right(content) =>
              val newContent = DependencyFileUpdate.replaceDependency(
                fileType,
                content,
                request.dependencyName,
                request.fromVersion,
                request.toVersion
              )
              if newContent == content
              then s"Failed to change file content".asLeft
              else newContent.asRight

      private def ensureAttemptWasNotMadeBefore(
          request: UpdateDependencyDetails
      ) =
        repository
          .exists(
            request.projectId,
            request.dependencyName,
            request.toVersion
          )
          .flatMap:
            case true  => "Update request already exists".asLeft.pure
            case false => ().asRight.pure

      private def publishToGit(
          request: UpdateDependencyDetails,
          newContent: String
      ) =
        val newBranchName =
          s"ganyu-${request.dependencyName}-${request.toVersion}"
        val commitActions =
          List(CommitAction(Action.Update, request.filePath, newContent))
        val commitMessage =
          s"Bumps ${request.dependencyName} from ${request.fromVersion} to ${request.toVersion}"
        val mergeRequestTitle = commitMessage

        gitlabApi.createBranch(
          request.projectGitlabId,
          request.projectBranch,
          newBranchName
        )
          *> gitlabApi.createCommit(
            request.projectGitlabId,
            newBranchName,
            commitMessage,
            commitActions
          )
          *> gitlabApi.createMergeRequest(
            request.projectGitlabId,
            newBranchName,
            request.projectBranch,
            mergeRequestTitle
          )

      private def getFileContent(
          projectGitlabId: String,
          branch: String,
          pathToFile: String
      ) =
        // TODO: Move it to GitlabApi?
        gitlabApi
          .getFile(projectGitlabId, branch, pathToFile)
          .map: fileResult =>
            fileResult.flatMap: file =>
              GitlabApi.decodeContent(file.content)

  enum FileType:
    case Txt, Toml
  object FileType:
    def fromPath(path: String): Either[String, FileType] =
      path.split("[.]").lastOption match
        case Some("txt") => FileType.Txt.asRight
        // "temporarily" disable toml support -- requires additional poetry.lock handling
        // case Some("toml") => FileType.Toml.asRight
        case other => s"$other is not a supported format".asLeft

  object DependencyFileUpdate:
    def replaceDependency(
        fileType: FileType,
        fileContent: String,
        name: String,
        from: String,
        to: String
    ): String =
      val symbollessFrom = from.replaceAll(removeSymbolsRegex, "")
      fileType match
        case FileType.Txt =>
          replaceDependencyInTxt(fileContent, name, symbollessFrom, to)
        case FileType.Toml =>
          replaceDependencyInToml(fileContent, name, symbollessFrom, to)

    private def replaceDependencyInTxt(
        fileContent: String,
        name: String,
        from: String,
        to: String
    ): String =
      fileContent
        .split("\n")
        .map: line =>
          val index                = line.indexOf(name)
          val indexOfCharAfterName = index + name.length
          val isLineNameAndVersion = index == 0
            && line.length > indexOfCharAfterName
            && versionComparisonSymbols.contains(line(indexOfCharAfterName))
          if isLineNameAndVersion then
            line.replace(from, to)
          else
            line
        .mkString("\n") + "\n"

    private def replaceDependencyInToml(
        fileContent: String,
        name: String,
        from: String,
        to: String
    ): String =
      fileContent
        .split("\n")
        .map: line =>
          val index                = line.indexOf(name)
          val indexOfCharAfterName = index + name.length
          val isLineNameAndVersion = index == 0
            && line.length > indexOfCharAfterName
            && line(indexOfCharAfterName) == ' '
          if isLineNameAndVersion then
            line.replace(from, to)
          else
            line
        .mkString("\n") + "\n"

    private val versionComparisonSymbols = List('=', '>', '^', '~')
    private val removeSymbolsRegex =
      versionComparisonSymbols.mkString("[", "", "]")
