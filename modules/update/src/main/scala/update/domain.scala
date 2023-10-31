package update

import java.util.UUID

object domain:
  enum FileType:
    case Txt, Toml

  case class UpdateDependency(
      projectName: String,
      dependencyName: String,
      filePath: String,
      fromVersion: String,
      toVersion: String
  )

  case class UpdateDependencyDetails(
      projectId: UUID,
      projectBranch: String,
      projectGitlabId: String,
      filePath: String,
      dependencyName: String,
      fromVersion: String,
      toVersion: String
  )

  case class UpdateAttempt(
      projectId: UUID,
      dependencyName: String,
      toVersion: String,
      mergeRequestUrl: String
  )

  trait UpdateRepository[F[_]]:
    def exists(
        projectId: UUID,
        dependencyName: String,
        toVersion: String
    ): F[Boolean]
    def save(attempt: UpdateAttempt): F[UUID]

  trait UpdateService[F[_]]:
    def update(request: UpdateDependency): F[Either[String, Unit]]
    def update(request: UpdateDependencyDetails): F[Either[String, Unit]]
