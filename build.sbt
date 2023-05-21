import Dependencies.Libraries

ThisBuild / scalafixDependencies += Libraries.organizeImports
ThisBuild / scalaVersion := "3.2.0"

Global / semanticdbEnabled := true
Global / semanticdbVersion := scalafixSemanticdb.revision

lazy val root = project
  .in(file("."))
  .settings(
    name    := "sentinel",
    version := "0.5.0",
    libraryDependencies ++= Seq(
      Libraries.sttp,
      Libraries.sttpCats,
      Libraries.sttpCirce,
      Libraries.circe,
      Libraries.circeGeneric,
      Libraries.spoiwo,
      Libraries.log4j,
      Libraries.toml4j,
      Libraries.catsCore,
      Libraries.catsEffect,
      Libraries.scalaTime,
      Libraries.ciris,
      Libraries.decline,
      Libraries.declineCatsEffect,
      Libraries.doobie,
      Libraries.doobieHikari,
      Libraries.sqliteJDB,
      Libraries.woof,
      Libraries.scalaTestDiscipline % Test,
      Libraries.scalaTestCatsEffect % Test
    )
  )

enablePlugins(JavaAppPackaging)

addCommandAlias("lint", ";scalafmtAll ;scalafixAll --rules OrganizeImports")
