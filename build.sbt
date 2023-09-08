import Dependencies.Libraries

ThisBuild / scalafixDependencies += Libraries.organizeImports
ThisBuild / scalaVersion := "3.3.0"

Global / semanticdbEnabled := true
Global / semanticdbVersion := scalafixSemanticdb.revision

val commonSettings = List(
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
    Libraries.http4sDsl,
    Libraries.http4sServer,
    Libraries.http4sCirce,
    Libraries.doobie,
    Libraries.scalaTags,
    Libraries.doobieHikari,
    Libraries.sqliteJDB,
    Libraries.woof,
    Libraries.scalaTestDiscipline % Test,
    Libraries.scalaTestCatsEffect % Test,
    Libraries.doobieScalaTest     % Test
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name    := "ganyu",
    version := "0.8.5",
    commonSettings
  )
  .aggregate(core, gitlab, parsers, scanning, upkeep)
  .dependsOn(core, gitlab, parsers, scanning, upkeep)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings: _*)

lazy val gitlab = project
  .in(file("modules/gitlab"))
  .settings(commonSettings: _*)

lazy val parsers = project
  .in(file("modules/parsers"))
  .settings(commonSettings: _*)
  .dependsOn(core)

lazy val processor = project
  .in(file("modules/processor"))
  .settings(commonSettings: _*)
  .dependsOn(core)

lazy val persistence = project
  .in(file("modules/persistence"))
  .settings(commonSettings: _*)
  .dependsOn(core)

lazy val scanning = project
  .in(file("modules/scanning"))
  .settings(commonSettings: _*)
  .dependsOn(core, parsers, gitlab, processor, persistence)

lazy val upkeep = project
  .in(file("modules/upkeep"))
  .settings(commonSettings: _*)
  .dependsOn(core, parsers, gitlab, persistence)

enablePlugins(sbtdocker.DockerPlugin, JavaAppPackaging)

def mapFiles(root: File): List[(File, String)] =
  if (root.isDirectory) {
    root
      .listFiles()
      .toList
      .flatMap(mapFiles _)
  } else {
    List(root -> root.asPath.toString)
  }

Universal / mappings ++= mapFiles(file("static")).toSeq
Universal / mappings ++= mapFiles(file("db")).toSeq

docker / dockerfile := {
  val appDir: File = stage.value
  val targetDir    = "/app"

  new Dockerfile {
    from("public.ecr.aws/docker/library/openjdk:17-jdk")
    workDir(targetDir)
    entryPoint(s"./bin/${executableScriptName.value}")
    copy(appDir, targetDir, chown = "daemon:daemon")
  }
}

addCommandAlias("lint", ";scalafmtAll ;scalafixAll --rules OrganizeImports")
