package update

import java.util.UUID

object domain:
  enum FileType:
    case Txt, Toml

  /**
   * NOTE:
   * This properties can all be extracted from UI.
   * When making a POST action button, add these props to js-vals htmx property
   * or something
   */
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
    /* This should store the requests in DB,
     * to prevent making duplicate MRs
     */
    def exists(
        projectId: UUID,
        dependencyName: String,
        toVersion: String
    ): F[Boolean]
    def save(attempt: UpdateAttempt): F[UUID]


  trait UpdateService[F[_]]:
    def update(request: UpdateDependencyDetails): F[Either[String, Unit]]
