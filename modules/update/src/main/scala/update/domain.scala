package update

import java.util.UUID
import cats.syntax.all.*

object domain:
  enum FileType:
    case Txt, Toml
  object FileType:
    def fromPath(path: String): Either[String, FileType] =
      path.split("[.]").lastOption match
        case Some("txt") => FileType.Txt.asRight
        // "temporarily" disable toml support -- requires additional poetry.lock handling
        // case Some("toml") => FileType.Toml.asRight
        case other => s"$other is not a supported format".asLeft

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
