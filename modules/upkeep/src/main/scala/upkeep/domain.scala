package upkeep

import org.joda.time.DateTime

object domain:
  case class UpdateDependency[A](
      projectId: A,
      sourceBranch: String,
      filePath: String,
      name: String,
      from: String,
      to: String
  )

  def replaceDependency(
      fileContent: String,
      name: String,
      from: String,
      to: String
  ): String =
    fileContent
      .split(NEWLINE)
      .map(line => if line.contains(name) then line.replace(from, to) else line)
      .mkString(NEWLINE)

  trait UpkeepService[F[_], A]:
    def updateProject(command: UpdateDependency[A]): F[Either[String, Unit]]

  trait ProjectDependencyRepository[F[_], A]:
    def getAffectedProjects(
        dependencyName: String
    ): F[List[UpdateDependency[A]]]

  case class UpkeepRequest[A](
      projectId: A,
      dependencyName: String,
      updateToVersion: String
  )

  trait UpkeepRepository[F[_], A]:
    def save(): F[Unit]
    def isPending(request: UpkeepRequest[A]): F[Boolean]

  private val NEWLINE = "\n"
