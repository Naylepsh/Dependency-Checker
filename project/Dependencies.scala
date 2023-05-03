import sbt._

object Dependencies {
  object V {
    val sttp       = "3.8.15"
    val spoiwo     = "2.2.1"
    val toml4j     = "0.7.2"
    val catsCore   = "2.9.0"
    val catsEffect = "3.4.9"
    val circe      = "0.14.5"

    val log4j = "2.20.0"
    val woof  = "0.6.0"

    val scalaTestDiscipline = "2.2.0"
    val scalaTestCatsEffect = "1.5.0"

    val organizeImports = "0.6.0"
  }

  object Libraries {
    val sttp = "com.softwaremill.sttp.client3" %% "core" % V.sttp
    val sttpCats =
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-cats" % V.sttp
    val sttpCirce    = "com.softwaremill.sttp.client3" %% "circe"         % V.sttp
    val circe        = "io.circe"                      %% "circe-core"    % V.circe
    val circeGeneric = "io.circe"                      %% "circe-generic" % V.circe
    val spoiwo       = "com.norbitltd"                 %% "spoiwo"        % V.spoiwo
    val toml4j       = "com.moandjiezana.toml"          % "toml4j"        % V.toml4j
    val catsCore     = "org.typelevel"                 %% "cats-core"     % V.catsCore
    val catsEffect   = "org.typelevel"                 %% "cats-effect"   % V.catsEffect

    // logging
    val log4j = "org.apache.logging.log4j" % "log4j-core" % V.log4j
    val woof  = "org.legogroup"           %% "woof-core"  % V.woof

    // test
    val scalaTestDiscipline =
      "org.typelevel" %% "discipline-scalatest" % V.scalaTestDiscipline
    val scalaTestCatsEffect =
      "org.typelevel" %% "cats-effect-testing-scalatest" % V.scalaTestCatsEffect

    // scalafix rules
    val organizeImports =
      "com.github.liancheng" %% "organize-imports" % V.organizeImports
  }
}
