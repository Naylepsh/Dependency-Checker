package domain

import scala.util._

object semver {
  enum VersionDifference {
    case Patch, Minor, Major
  }

  def calculateVersionDifference(
      a: String,
      b: String
  ): Option[VersionDifference] = {
    if (a == b) None
    else {
      val res = for {
        (aSymbol, aMajor, aMinor, aPatch) <- extractVersion(a)
        (_, bMajor, bMinor, bPatch) <- extractVersion(b)
      } yield {
        if (aMajor != bMajor)
          Some(VersionDifference.Major)
        else if (aMajor == bMajor && aSymbol == Some("^"))
          None
        else if (aMinor != bMinor)
          Some(VersionDifference.Minor)
        else if (aMinor == bMinor && aSymbol == Some("~"))
          None
        else if (aPatch != bPatch)
          Some(VersionDifference.Patch)
        else
          None
      }
      res.flatten
    }
  }

  private def extractVersion(
      text: String
  ): Option[
    (Option[String], Option[String], Option[String], Option[String])
  ] = {
    versionPattern
      .findFirstMatchIn(text)
      .map(matches => {
        val symbol = Option(matches.group(1))
        val major = Option(matches.group(2))
        val minor = Option(matches.group(3))
        val patch = Option(matches.group(4))

        (symbol, major, minor, patch)
      })
  }

  private val versionPattern = "([~^])*([0-9])*.([0-9a-z])*.?([0-9a-z])*".r
}
