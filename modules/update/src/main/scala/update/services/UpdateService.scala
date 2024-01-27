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
import parsers.python.{ PackageManagementFiles, Poetry, Requirements }

private case class FileContent(filePath: String, content: String)

object UpdateService:
  def make[F[_]: Monad: Logger](
      repository: UpdateRepository[F],
      projectConfigRepository: ProjectScanConfigRepository[F],
      gitlabApi: GitlabApi[F],
      jiraNotificationService: JiraNotificationService[F],
      poetry: Poetry[F],
      requirements: Requirements
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
                updatePackage(request, fileType)
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
        contents: List[FileContent]
    ) =
      val newBranchName =
        s"ganyu-${request.dependencyName}-${request.toVersion}"
      val commitActions = contents.map: fileContent =>
        CommitAction(Action.Update, fileContent.filePath, fileContent.content)
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

    private def updatePackage(
        request: UpdateDependencyDetails,
        fileType: FileType
    ) =
      fileType match
        case FileType.Txt  => updatePackageInRequirements(request)
        case FileType.Toml => updatePackageInPoetry(request)

    private def updatePackageInRequirements(request: UpdateDependencyDetails) =
      EitherT:
        getRequirementsTxtFromGit(
          request.projectGitlabId,
          request.projectBranch,
          request.filePath
        ).map: result =>
          result
            .flatMap: originalFile =>
              requirements
                .update(
                  request.dependencyName,
                  request.fromVersion,
                  request.toVersion,
                  originalFile
                )
                .map: updatedFile =>
                  FileContent(request.filePath, updatedFile.content) :: Nil
            .leftMap(_.toString)

    private def getRequirementsTxtFromGit(
        projectGitlabId: String,
        projectBranch: String,
        requirementsPath: String
    ): F[Either[String, PackageManagementFiles.RequirementFile]] =
      gitlabApi
        .getFileContent(projectGitlabId, projectBranch, requirementsPath)
        .map: result =>
          result.map: content =>
            PackageManagementFiles.RequirementFile(content)

    private def updatePackageInPoetry(request: UpdateDependencyDetails) =
      val parent    = Paths.get(request.filePath).getParent
      val pyProject = parent.resolve("pyproject.toml").toString
      val lock      = parent.resolve("poetry.lock").toString

      for
        originalFiles <- EitherT(getPoetryFilesFromGit(
          request.projectGitlabId,
          request.projectBranch,
          pyProject,
          lock
        ))
        updatedFiles <- EitherT(poetry.update(
          request.dependencyName,
          request.fromVersion,
          request.toVersion,
          originalFiles
        ).map(_.leftMap(_.toString)))
        commits =
          List(
            FileContent(pyProject, updatedFiles.pyProjectContent),
            FileContent(lock, updatedFiles.lockContent)
          )
      yield commits

    private def getPoetryFilesFromGit(
        projectGitlabId: String,
        projectBranch: String,
        pyProjectPath: String,
        lockPath: String
    ): F[Either[String, PackageManagementFiles.PoetryFiles]] =
      val getPyProject = gitlabApi.getFileContent(
        projectGitlabId,
        projectBranch,
        pyProjectPath
      )
      val getLock = gitlabApi.getFileContent(
        projectGitlabId,
        projectBranch,
        lockPath
      )

      (getPyProject, getLock).tupled.map: (pyProjectRes, lockRes) =>
        (pyProjectRes, lockRes).tupled.map: (pyProject, lock) =>
          PackageManagementFiles.PoetryFiles(pyProject, lock)
