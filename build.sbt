import Dependencies.Libraries

ThisBuild / scalafixDependencies += Libraries.organizeImports
ThisBuild / scalaVersion := "3.2.0"

Global / semanticdbEnabled := true
Global / semanticdbVersion := scalafixSemanticdb.revision

lazy val root = project
  .in(file("."))
  .settings(
    name    := "sentinel",
    version := "0.2.0",
    libraryDependencies ++= Seq(
      Libraries.requests,
      Libraries.upickle,
      Libraries.spoiwo,
      Libraries.log4j,
      Libraries.toml4j,
      Libraries.catsCore,
      Libraries.catsEffect,
      Libraries.woof,
      Libraries.disciplineScalaTest % Test
    )
  )

enablePlugins(JavaAppPackaging)

addCommandAlias("lint", ";scalafmtAll ;scalafixAll --rules OrganizeImports")
