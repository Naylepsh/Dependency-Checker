import Dependencies.Libraries

ThisBuild / scalafixDependencies += Libraries.organizeImports
ThisBuild / scalaVersion := "3.3.1"

Global / semanticdbEnabled := true
Global / semanticdbVersion := scalafixSemanticdb.revision

val commonSettings = List(
  libraryDependencies ++= Seq(
    Libraries.sttp,
    Libraries.sttpCats,
    Libraries.sttpCirce,
    Libraries.circe,
    Libraries.circeGeneric,
    Libraries.toml4j,
    Libraries.catsCore,
    Libraries.catsEffect,
    Libraries.kittens,
    Libraries.monocle,
    Libraries.scalaTime,
    Libraries.ciris,
    Libraries.http4sDsl,
    Libraries.http4sServer,
    Libraries.http4sCirce,
    Libraries.doobie,
    Libraries.scalaTags,
    Libraries.scalaScraper,
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
    version := "0.13.6",
    fork    := true,
    commonSettings
  )
  .aggregate(core, advisory, jira, gitlab, parsers, scanning, update)
  .dependsOn(core, advisory, jira, gitlab, parsers, scanning, update)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings: _*)

lazy val advisory = project
  .in(file("modules/advisory"))
  .settings(commonSettings: _*)
  .dependsOn(core)

lazy val gitlab = project
  .in(file("modules/gitlab"))
  .settings(commonSettings: _*)

lazy val jira = project
  .in(file("modules/jira"))
  .settings(commonSettings: _*)
  .dependsOn(core)

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
  .dependsOn(core, advisory, parsers, gitlab, processor, persistence)

lazy val update = project
  .in(file("modules/update"))
  .settings(commonSettings: _*)
  .dependsOn(core, parsers, jira, gitlab, persistence)

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
Universal / mappings ++= mapFiles(file("templates/jira")).toSeq

docker / dockerfile := {
  val appDir: File = stage.value
  val targetDir    = "/app"

  new Dockerfile {
    from("public.ecr.aws/docker/library/openjdk:17-jdk")
    workDir(targetDir)
    run(
      "curl",
      "-fsSL",
      "-o",
      "/usr/local/bin/dbmate",
      "https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-amd64"
    )
    run("chmod", "+x", "/usr/local/bin/dbmate")
    entryPoint(s"./bin/${executableScriptName.value}")
    copy(appDir, targetDir, chown = "daemon:daemon")
  }
}

addCommandAlias("lint", ";scalafmtAll ;scalafixAll --rules OrganizeImports")
