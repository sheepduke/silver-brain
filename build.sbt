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
  "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion
)

val libsHttpContract = Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % tapirVersion
)

val libHttpClient = "org.http4s" %% "http4s-ember-client" % http4sVersion

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
//  Silver Brain
// ============================================================

lazy val silverBrain = project
  .in(file("modules/silver-brain"))
  .settings(
    name := "silver-brain",
    libraryDependencies ++= Seq(
      libCliArgsParser,
      libLoggerImplementation
    )
  )
  .dependsOn(silverBrainHttpServer)
  .aggregate(silverBrainHttpServer)

// ============================================================
//  Http Server
// ============================================================

lazy val silverBrainHttpServer = project
  .in(file("modules/http-server"))
  .settings(
    name := "silver-brain-http-server",
    libraryDependencies ++= Seq(
      libLoggerInterface,
      libLoggerImplementation
    ) ++ libsHttpServer ++ libsJson ++ libsTestFramework
  )
  .dependsOn(silverBrainHttpContract, silverBrainStore)
  .aggregate(silverBrainHttpContract, silverBrainStore)
  .enablePlugins(JavaAppPackaging)

// ============================================================
//  Http Contract
// ============================================================

lazy val silverBrainHttpContract = project
  .in(file("modules/http-contract"))
  .settings(
    name := "silver-brain-http-contract",
    libraryDependencies ++= Seq() ++ libsHttpContract ++ libsJson
  )
  .dependsOn(silverBrainCore)
  .aggregate(silverBrainCore)

// ============================================================
//  Store
// ============================================================

lazy val silverBrainStore = project
  .in(file("modules/store"))
  .settings(
    name := "silver-brain-store",
    libraryDependencies ++= Seq(
      libUniqueId,
      libOsLib
    ) ++ libsDatabase ++ libsTestFramework
  )
  .dependsOn(silverBrainCore)
  .aggregate(silverBrainCore)

// ============================================================
//  Core
// ============================================================

lazy val silverBrainCore =
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

lazy val silverBrainPlayground =
  project
    .in(file("tools/playground"))
    .settings(
      name := "silver-brain-playground",
      libraryDependencies ++= Seq(
        libUniqueId,
        libOsLib
      ) ++ libsDatabase ++ libsJson
    )
    .dependsOn(silverBrainCore)
    .aggregate(silverBrainCore)
