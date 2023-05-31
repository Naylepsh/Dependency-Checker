package upkeep

import cats.Monad
import cats.effect.MonadCancelThrow
import cats.implicits.*
import core.infra.GitlabApi
import upkeep.domain.*
import cats.data.EitherT
import org.legogroup.woof.{ Logger, given }
import core.domain.registry.RegistryRepository
import org.joda.time.DateTime
import core.domain.registry.Registry

object application:
  object UpkeepService:
    def makeForGitlab[F[_]: Monad: Logger](
        api: GitlabApi[F],
        repository: ProjectDependencyRepository[F, String],
        upkeepRepository: UpkeepRepository[F, String]
    ): UpkeepService[F, String] = new:
      override def updateProject(command: UpdateDependency[String])
          : F[Either[String, Unit]] = updateProjectInner(command).map(_.void)

      private def updateProjectInner(command: UpdateDependency[String])
          : F[Either[String, core.infra.CreateMergeRequestResponse]] =
        (for
          file    <- getFile(command)
          content <- decodeContent(file.content)
          newContent =
            replaceDependency(content, command.name, command.from, command.to)
          result <- if newContent == content then
            makeFailedToChangeContentError(command)
          else
            val targetBranch = s"sentinel/${command.name}-${command.to}"
            submitUpdate(command, targetBranch, content)
        yield result).value.flatTap {
          case Left(reason) => Logger[F].error(reason)
          case Right(_)     => ().pure
        }

      private def getFile(command: UpdateDependency[String]) =
        EitherT(api.getFile(
          command.projectId,
          command.sourceBranch,
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
          targetBranch: String,
          content: String
      ) =
        val commitMessage =
          s"Bumps ${command.name} from ${command.from} to ${command.to}"
        val mergeRequestTitle = commitMessage

        EitherT(api.createBranch(
          command.projectId,
          command.sourceBranch,
          targetBranch
        ))
          *> EitherT(api.createCommit(
            command.projectId,
            targetBranch,
            commitMessage,
            List(core.infra.CommitAction(
              core.infra.Action.Update,
              command.filePath,
              content
            ))
          ))
          *> EitherT(api.createMergeRequest(
            command.projectId,
            command.sourceBranch,
            targetBranch,
            mergeRequestTitle
          ))

      def updateAffectedProjects(dependencyName: String)
          : F[List[Either[String, Unit]]] =
        repository
          .getAffectedProjects(dependencyName)
          .flatMap(_.traverse(project =>
            upkeepRepository.isPending(
              project.projectId,
              project.name,
              project.to
            ).flatMap {
              case true => ().asRight.pure
              case false =>
                updateProjectInner(project).flatMap {
                  case Left(reason) => reason.asLeft.pure
                  case Right(mergeRequest) =>
                    upkeepRepository.save(UpkeepRequest(
                      project.projectId,
                      project.name,
                      project.to,
                      mergeRequest.webUrl
                    )).map(_.asRight)
                }

            }
          ))
