package core.domain

import cats.syntax.all.*
import scala.util.*

object semver:
  enum VersionDifference:
    case Patch, Minor, Major

  def calculateVersionDifference(
      a: String,
      b: String
  ): Option[VersionDifference] =
    if a == b then None
    else
      val res =
        for
          (aSymbol, aMajor, aMinor, aPatch) <- extractVersion(a)
          (_, bMajor, bMinor, bPatch)       <- extractVersion(b)
        yield
          if aMajor != bMajor then
            Some(VersionDifference.Major)
          else if aMajor == bMajor && aSymbol == Some("^") then
            None
          else if aMinor != bMinor then
            Some(VersionDifference.Minor)
          else if aMinor == bMinor && aSymbol == Some("~") then
            None
          else if aPatch != bPatch then
            Some(VersionDifference.Patch)
          else
            None
      res.flatten

  def isHoled(version: String): Boolean = version.contains("*")

  def unhole(version: String): Option[String] =
    extractVersion(version.replaceAll("[*]", "0"))
      .flatMap: (symbol, major, minor, patch) =>
        (major, minor, patch).tupled.map: (major, minor, patch) =>
          s"""${symbol.getOrElse("")}$major.$minor.$patch"""

  def removeSymbol(version: String): String = version.replaceAll("[~^]", "")

  private def extractVersion(
      text: String
  ): Option[
    (Option[String], Option[String], Option[String], Option[String])
  ] =
    versionPattern
      .findFirstMatchIn(text)
      .map(matches =>
        val symbol = Option(matches.group(1))
        val major  = Option(matches.group(2))
        val minor  = Option(matches.group(3))
        val patch  = Option(matches.group(4))

        (symbol, major, minor, patch)
      )

  private val versionPattern = "([~^])*([0-9])*.([0-9a-z])*.?([0-9a-z])*".r
