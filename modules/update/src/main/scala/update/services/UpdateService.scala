package update.services

import java.nio.file.Paths

import cats.data.EitherT
import cats.syntax.all.*
import cats.{ Applicative, Monad }
import core.domain.project.ProjectScanConfigRepository
import core.domain.update.UpdateDependency
import gitlab.{ Action, CommitAction, GitlabApi }
import jira.*
import update.domain.*
import org.legogroup.woof.{ *, given }
import parsers.python.PackageManagementFiles

object UpdateService:
  def make[F[_]: Monad: Logger](
      repository: UpdateRepository[F],
      projectConfigRepository: ProjectScanConfigRepository[F],
      gitlabApi: GitlabApi[F],
      jiraNotificationService: JiraNotificationService[F]
  ): UpdateService[F] = new:

    def update(request: UpdateDependency): F[Either[String, Unit]] =
      projectConfigRepository
        .findByProjectName(request.projectName)
        .flatMap:
          case None => "Config for project not found".asLeft.pure
          case Some(config) =>
            val req = UpdateDependencyDetails(
              projectId = config.project.id,
              projectName = config.project.name,
              projectBranch = config.branch,
              projectGitlabId = config.project.repositoryId,
              filePath = request.filePath,
              dependencyName = request.dependencyName,
              fromVersion = request.fromVersion,
              toVersion = request.toVersion
            )
            update(req)

    def update(request: UpdateDependencyDetails): F[Either[String, Unit]] =
      Logger[F].info("Requested update") *> (
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
                  mergeRequest.webUrl.toString
                )
                EitherT(repository.save(attempt).map(_.asRight))
              .flatTap: mergeRequest =>
                EitherT(
                  jiraNotificationService.notify(request, mergeRequest.webUrl)
                )
              .flatTap: _ =>
                EitherT(Logger[F].info("Done with update").map(_.asRight))
              .void
              .value
      )

    private def updateDependencyFile(
        request: UpdateDependencyDetails,
        fileType: FileType
    ): F[Either[String, String]] =
      gitlabApi.getFileContent(
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

    private def getPackageManagementFilesFromGit(
        fileType: FileType,
        request: UpdateDependencyDetails
    ): F[Either[String, PackageManagementFiles]] =
      fileType match
        case FileType.Txt  => getRequirementsTxtFromGit(request)
        case FileType.Toml => getPoetryFilesFromGit(request)

    private def getRequirementsTxtFromGit(request: UpdateDependencyDetails)
        : F[Either[String, PackageManagementFiles]] =
      gitlabApi.getFileContent(
        request.projectGitlabId,
        request.projectBranch,
        request.filePath
      ).map: result =>
        result.map: content =>
          PackageManagementFiles.RequirementFile(content)

    private def getPoetryFilesFromGit(request: UpdateDependencyDetails)
        : F[Either[String, PackageManagementFiles]] =
      val parent = Paths.get(request.filePath).getParent

      val getPyProject = gitlabApi.getFileContent(
        request.projectGitlabId,
        request.projectBranch,
        parent.resolve("pyproject.toml").toString
      )
      val getLock = gitlabApi.getFileContent(
        request.projectGitlabId,
        request.projectBranch,
        parent.resolve("poetry.lock").toString
      )

      (getPyProject, getLock).tupled.map: (pyProjectRes, lockRes) =>
        (pyProjectRes, lockRes).tupled.map: (pyProject, lock) =>
          PackageManagementFiles.PoetryFiles(pyProject, lock)

    // TODO: Update PackageManagementFiles accordingly
