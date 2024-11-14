ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.sheepduke"

// ============================================================
//  Dependencies
// ============================================================

// CLI option parser.
val libCliArgsParser = "org.rogach" %% "scallop" % "5.1.0"

// HTTP server.
// val libHttpServer = "com.lihaoyi" %% "cask" % "0.9.2"
val http4sVersion = "0.23.29"
val tapirVersion = "1.11.8"
val libsHttpServer = Seq(
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % tapirVersion
)

val libsHttpClient = Seq(
  "org.http4s" %% "http4s-ember-client" % http4sVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-http4s-client" % tapirVersion
)

// JSON.
val libsJson = Seq(
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.28.4",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.28.4"
)

// Unique ID.
val libUniqueId = "com.github.ksuid" % "ksuid" % "1.1.2"

// Parser combinator.
val libParserCombinator = "com.lihaoyi" %% "fastparse" % "3.1.1"

// OS interaction.
val libOsLib = "com.lihaoyi" %% "os-lib" % "0.11.1"

// Effect system.
val libEffect = "org.typelevel" %% "cats-effect" % "3.5.5"

// Database access.
val doobieVersion = "1.0.0-RC4"
val libsDatabase = Seq(
  // "org.scalikejdbc" %% "scalikejdbc" % "4.0.0",
  "org.xerial" % "sqlite-jdbc" % "3.45.2.0",
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari" % doobieVersion,
  "org.tpolecat" %% "doobie-scalatest" % doobieVersion % Test,
  "org.flywaydb" % "flyway-core" % "9.0.4"
)

// Logging.
val libLoggerInterface = "org.slf4j" % "slf4j-api" % "2.0.13"
val libLoggerImplementation = "ch.qos.logback" % "logback-classic" % "1.3.6"

// Test.
val libsTestFramework = Seq(
  "org.scalactic" %% "scalactic" % "3.2.19" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

// ============================================================
//  Server
// ============================================================

lazy val server = project
  .in(file("modules/server"))
  .settings(
    name := "silver-brain-http-server",
    libraryDependencies ++= Seq(
      libLoggerInterface,
      libLoggerImplementation
    ) ++ libsHttpServer ++ libsJson ++ libsTestFramework
  )
  .dependsOn(store)
  .aggregate(store)
  .enablePlugins(JavaAppPackaging)

// ============================================================
//  Client Http
// ============================================================

lazy val clientHttp = project
  .in(file("modules/client-http"))
  .settings(
    name := "silver-brain-client-http",
    libraryDependencies ++= libsHttpClient ++ libsJson
  )
  .dependsOn(core)
  .dependsOn(core)

// ============================================================
//  Store
// ============================================================

lazy val store = project
  .in(file("modules/store"))
  .settings(
    name := "silver-brain-store",
    libraryDependencies ++= Seq(
      libUniqueId,
      libOsLib
    ) ++ libsDatabase ++ libsTestFramework
  )
  .dependsOn(core)
  .aggregate(core)

// ============================================================
//  Core
// ============================================================

lazy val core =
  project
    .in(file("modules/core"))
    .settings(
      name := "silver-brain-core",
      libraryDependencies ++= Seq(
        libParserCombinator,
        libEffect
      ) ++ libsTestFramework
    )

// ============================================================
//  Playground
// ============================================================

lazy val playground =
  project
    .in(file("tools/playground"))
    .settings(
      name := "silver-brain-playground",
      libraryDependencies ++= Seq(
        libUniqueId,
        libOsLib
      ) ++ libsDatabase ++ libsJson
    )
    .dependsOn(core)
    .aggregate(core)
