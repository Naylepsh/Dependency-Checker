val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dependency-checker",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.lihaoyi" %% "requests" % "0.7.0",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "1.6.0",

    // https://mvnrepository.com/artifact/com.norbitltd/spoiwo
    libraryDependencies += "com.norbitltd" %% "spoiwo" % "2.2.1",

    // https://mvnrepository.com/artifact/org.apache.logging.log4j/log4j-core
    libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.17.2",

    // https://mvnrepository.com/artifact/com.moandjiezana.toml/toml4j
    libraryDependencies += "com.moandjiezana.toml" % "toml4j" % "0.7.2",

    // https://mvnrepository.com/artifact/org.typelevel/cats-core
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",

    // https://mvnrepository.com/artifact/org.typelevel/discipline-scalatest
    libraryDependencies += "org.typelevel" %% "discipline-scalatest" % "2.1.5" % Test
  )
