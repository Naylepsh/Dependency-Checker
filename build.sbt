val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dependency-checker",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    // https://mvnrepository.com/artifact/org.typelevel/discipline-scalatest
    libraryDependencies += "org.typelevel" %% "discipline-scalatest" % "2.1.5" % Test
  )
