package upkeep.application

import cats.Monad
import cats.data.EitherT
import cats.effect.MonadCancelThrow
import cats.implicits.*
import core.domain.registry.{ Registry, RegistryRepository }
import gitlab.*
import org.joda.time.DateTime
import org.legogroup.woof.{ Logger, given }
import upkeep.domain.*

object UpkeepService:
  def makeForGitlab[F[_]: Monad: Logger](
      api: GitlabApi[F],
      projectRepository: ProjectDependencyRepository[F, String],
      upkeepRepository: UpkeepRepository[F, String]
  ): UpkeepService[F, String] = new:
    override def updateProject(command: UpdateDependency[String])
        : F[Either[String, Unit]] = updateProjectInner(command).map(_.void)

    private def updateProjectInner(command: UpdateDependency[String])
        : F[Either[String, CreateMergeRequestResponse]] =
      command.fileType match
        case Left(reason) => Logger[F].error(reason).as(Left(reason))
        case Right(fileType) =>
          val updateAttempt =
            for
              file    <- getFile(command)
              content <- decodeContent(file.content)
              newContent =
                replaceDependency(
                  fileType,
                  content,
                  command.name,
                  command.from,
                  command.to
                )
              result <- if newContent == content then
                makeFailedToChangeContentError(command)
              else
                val targetBranch = s"sentinel-${command.name}-${command.to}"
                submitUpdate(command, targetBranch, newContent)
            yield result

          updateAttempt.value.flatTap {
            case Left(reason) => Logger[F].error(reason)
            case Right(mergeRequest) =>
              Logger[F].info(
                s"Successfully submited update request ${mergeRequest.webUrl}"
              )
          }

    private def getFile(command: UpdateDependency[String]) =
      EitherT(api.getFile(
        command.projectId,
        command.branch,
        command.filePath
      ))

    private def decodeContent(encodedContent: String) =
      EitherT(
        GitlabApi.decodeContent(encodedContent)
          .leftMap(_.toString)
          .pure
      )

    private def makeFailedToChangeContentError(
        command: UpdateDependency[String]
    ) =
      EitherT(
        s"Failed to change the content for ${command.projectId}, ${command.name}, ${command.to}".asLeft.pure
      )

    private def submitUpdate(
        command: UpdateDependency[String],
        sourceBranch: String,
        content: String
    ) =
      val commitMessage =
        s"Bumps ${command.name} from ${command.from} to ${command.to}"
      val mergeRequestTitle = commitMessage

      EitherT(api.createBranch(
        command.projectId,
        command.branch,
        sourceBranch
      ))
        *> EitherT(Logger[F].debug("Created branch").map(_.asRight))
        *> EitherT(api.createCommit(
          command.projectId,
          sourceBranch,
          commitMessage,
          List(CommitAction(
            Action.Update,
            command.filePath,
            content
          ))
        ))
        *> EitherT(Logger[F].debug("Created commit").map(_.asRight))
        *> EitherT(api.createMergeRequest(
          command.projectId,
          sourceBranch,
          command.branch,
          mergeRequestTitle
        ))
        <* EitherT(Logger[F].debug("Created merge request").map(_.asRight))

    def updateAffectedProjects(dependencyName: String)
        : F[List[Either[String, Unit]]] =
      projectRepository
        .getAffectedProjects(dependencyName)
        .flatMap: projects =>
          projects.traverse(updateProjectIfHaventAttemptedBefore)

    private def updateProjectIfHaventAttemptedBefore(
        project: UpdateDependency[String]
    ) =
      upkeepRepository
        .isPending(project.projectId, project.name, project.to)
        .flatMap: isPending =>
          if isPending then
            ().asRight.pure
          else
            updateProjectInner(project)
              .flatMap: result =>
                result.traverse: mergeRequest =>
                  upkeepRepository.save(UpkeepRequest(
                    project.projectId,
                    project.name,
                    project.to,
                    mergeRequest.webUrl
                  ))
