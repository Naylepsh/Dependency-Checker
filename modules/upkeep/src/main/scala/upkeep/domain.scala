package upkeep

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
      .mkString(NEWLINE)

  private val versionComparisonSymbols = List('=', '>', '^', '~')
  private val NEWLINE                  = "\n"

  trait UpkeepService[F[_], A]:
    def updateProject(command: UpdateDependency[A]): F[Either[String, Unit]]

  trait ProjectDependencyRepository[F[_], A]:
    def getAffectedProjects(
        dependencyName: String
    ): F[List[UpdateDependency[A]]]

  case class UpkeepRequest[A](
      projectId: A,
      dependencyName: String,
      updateToVersion: String,
      url: String
  )

  trait UpkeepRepository[F[_], A]:
    def save(request: UpkeepRequest[A]): F[Unit]
    def isPending(
      projectId: A,
      dependencyName: String,
      updateToVersion: String,
    ): F[Boolean]
