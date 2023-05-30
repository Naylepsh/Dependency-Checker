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
          : F[Either[String, Unit]] =
        val targetBranch = s"sentinel/${command.name}-${command.to}"
        (for
          file <- EitherT(api.getFile(
            command.projectId,
            command.sourceBranch,
            command.filePath
          ))
          content <- EitherT(
            GitlabApi.decodeContent(file.content)
              .leftMap(_.toString)
              .pure
          )
          newContent =
            replaceDependency(content, command.name, command.from, command.to)
          _ <- EitherT(api.createBranch(command.projectId, targetBranch))
          _ <- EitherT(api.createMergeRequest(
            command.projectId,
            command.sourceBranch,
            targetBranch
          ))
        yield ()).value.flatTap {
          case Left(reason) => Logger[F].error(reason)
          case Right(_)     => ().pure
        }

      def updateAffectedProjects(dependencyName: String)
          : F[List[Either[String, Unit]]] =
        repository
          .getAffectedProjects(dependencyName)
          .flatMap(_.traverse(project =>
            val upkeepRequest = UpkeepRequest(
              project.projectId,
              project.name,
              project.to
            )
            upkeepRepository.isPending(upkeepRequest).flatMap {
              case true => ().asRight.pure
              case false =>
                updateProject(project)
                  *> upkeepRepository.save(upkeepRequest).map(_.asRight)
            }
          ))