import scala.util.Try
import scala.util.Success

package object Dependencies {
  case class Dependency(
      name: String,
      currentVersion: Option[String],
      latestVersion: Option[String]
  )

  case class RepositoryDependencies(
      name: String,
      dependencies: List[Dependency]
  )

  // TBD: Move this to a separate package?
  enum VersionDifference:
    case Patch, Minor, Major

  def calculate_version_difference(
      a: String,
      b: String
  ): Try[Option[VersionDifference]] = {
    if (a == b)
      return Success(None)

    val version = extractVersion(a).flatMap {
      case (aSymbol, aMajor, aMinor, aPatch) =>
        extractVersion(b).flatMap {
          case (_, bMajor, bMinor, bPatch) => {
            if (aMajor != bMajor)
              Some(VersionDifference.Major)
            else if (aMajor == bMajor && aSymbol == Some("^"))
              None
            else if (aMinor != bMinor )
              Some(VersionDifference.Minor)
            else if (aMinor == bMinor && aSymbol == Some("~"))
              None
            else if (aPatch != bPatch)
              Some(VersionDifference.Patch)
            else
              None
          }
        }
    }
    Success(version)
  }

  private def extractVersion(
      text: String
  ): Option[(Option[String], String, String, String)] = {
    versionPattern
      .findFirstMatchIn(text)
      .map(matches => {
        val symbol =
          if (matches.group(1) != null) Some(matches.group(1)) else None
        val major = matches.group(2)
        val minor = matches.group(3)
        val patch = matches.group(4)

        (symbol, major, minor, patch)
      })
  }

  private val versionPattern = "([~^])*([0-9])+.([0-9])+.([0-9])+".r

}
