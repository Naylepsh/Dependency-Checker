import sbt._

object Dependencies {
  object V {
    val requests   = "0.8.0"
    val upickle    = "3.1.0"
    val spoiwo     = "2.2.1"
    val toml4j     = "0.7.2"
    val catsCore   = "2.9.0"
    val catsEffect = "3.4.9"

    val log4j = "2.20.0"
    val woof  = "0.6.0"

    val disciplineScalaTest = "2.2.0"

    val organizeImports = "0.6.0"
  }

  object Libraries {
    val requests   = "com.lihaoyi"          %% "requests"    % V.requests
    val upickle    = "com.lihaoyi"          %% "upickle"     % V.upickle
    val spoiwo     = "com.norbitltd"        %% "spoiwo"      % V.spoiwo
    val toml4j     = "com.moandjiezana.toml" % "toml4j"      % V.toml4j
    val catsCore   = "org.typelevel"        %% "cats-core"   % V.catsCore
    val catsEffect = "org.typelevel"        %% "cats-effect" % V.catsEffect

    // logging
    val log4j = "org.apache.logging.log4j" % "log4j-core" % V.log4j
    val woof  = "org.legogroup"           %% "woof-core"  % V.woof

    // test
    val disciplineScalaTest = "org.typelevel" %% "discipline-scalatest" % V.disciplineScalaTest

    // scalafix rules
    val organizeImports = "com.github.liancheng" %% "organize-imports" % V.organizeImports
  }
}
