import sbt._

object Dependencies {
  object V {
    val sttp         = "3.9.0"
    val toml4j       = "0.7.2"
    val catsCore     = "2.10.0"
    val catsEffect   = "3.5.1"
    val kittens      = "3.0.0"
    val circe        = "0.14.6"
    val scalaTime    = "2.32.0"
    val ciris        = "3.2.0"
    val http4s       = "0.23.23"
    val scalaTags    = "0.12.0"
    val scalaScraper = "3.1.0"

    val doobie    = "1.0.0-RC2"
    val sqliteJDB = "3.43.0.0"

    val woof = "0.6.1"

    val scalaTestDiscipline = "2.2.0"
    val scalaTestCatsEffect = "1.5.0"

    val organizeImports = "0.6.0"
  }

  object Libraries {
    val sttp = "com.softwaremill.sttp.client3" %% "core" % V.sttp
    val sttpCats =
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-cats" % V.sttp
    val sttpCirce    = "com.softwaremill.sttp.client3" %% "circe"               % V.sttp
    val circe        = "io.circe"                      %% "circe-core"          % V.circe
    val circeGeneric = "io.circe"                      %% "circe-generic"       % V.circe
    val toml4j       = "com.moandjiezana.toml"          % "toml4j"              % V.toml4j
    val catsCore     = "org.typelevel"                 %% "cats-core"           % V.catsCore
    val catsEffect   = "org.typelevel"                 %% "cats-effect"         % V.catsEffect
    val kittens      = "org.typelevel"                 %% "kittens"             % V.kittens
    val scalaTime    = "com.github.nscala-time"        %% "nscala-time"         % V.scalaTime
    val ciris        = "is.cir"                        %% "ciris"               % V.ciris
    val http4sServer = "org.http4s"                    %% "http4s-ember-server" % V.http4s
    val http4sDsl    = "org.http4s"                    %% "http4s-dsl"          % V.http4s
    val http4sCirce  = "org.http4s"                    %% "http4s-circe"        % V.http4s
    val scalaTags    = "com.lihaoyi"                   %% "scalatags"           % V.scalaTags
    val scalaScraper = "net.ruippeixotog"              %% "scala-scraper"       % V.scalaScraper

    // db
    val doobie          = "org.tpolecat" %% "doobie-core"      % V.doobie
    val doobieHikari    = "org.tpolecat" %% "doobie-hikari"    % V.doobie
    val doobieScalaTest = "org.tpolecat" %% "doobie-scalatest" % V.doobie
    val sqliteJDB       = "org.xerial"    % "sqlite-jdbc"      % V.sqliteJDB

    // logging
    val woof = "org.legogroup" %% "woof-core" % V.woof

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
