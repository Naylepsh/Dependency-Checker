package upkeep

object domain:
  enum FileType:
    case Txt, Toml

  case class UpdateDependency[A](
      projectId: A,
      sourceBranch: String,
      filePath: String,
      name: String,
      from: String,
      to: String
  ):
    val fileType = filePath.split("[.]").lastOption match
      case Some("txt")  => Right(FileType.Txt)
      case Some("toml") => Right(FileType.Toml)
      case other        => Left(s"$other is not a supported format")

  def replaceDependency(
      fileType: FileType,
      fileContent: String,
      name: String,
      from: String,
      to: String
  ): String = fileType match
    case FileType.Txt  => replaceDependencyInTxt(fileContent, name, from, to)
    case FileType.Toml => replaceDependencyInToml(fileContent, name, from, to)

  private def replaceDependencyInTxt(
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

  private def replaceDependencyInToml(
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
          && line(indexOfCharAfterName) == ' '
        if isLineNameAndVersion then
          line.replace(from, to)
        else
          line
      .mkString(NEWLINE)

  private val versionComparisonSymbols = List('=', '>', '^', '~')
  private val NEWLINE                  = "\n"

  trait UpkeepService[F[_], A]:
    def updateProject(command: UpdateDependency[A]): F[Either[String, Unit]]
    def updateAffectedProjects(dependencyName: String)
        : F[List[Either[String, Unit]]]

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
        updateToVersion: String
    ): F[Boolean]
