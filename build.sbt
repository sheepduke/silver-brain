ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.sheepduke"

// ============================================================
//  Common Settings
// ============================================================

val commonSettings = Seq(
  Compile / scalaSource := baseDirectory.value / "src",
  Compile / resourceDirectory := baseDirectory.value / "resources",
  Test / scalaSource := baseDirectory.value / "test"
)

// ============================================================
//  Dependencies
// ============================================================

// CLI option parser.
val libCliArgsParser = "org.rogach" %% "scallop" % "5.1.0"

// HTTP server.
val libHttpServer = "com.lihaoyi" %% "cask" % "0.9.2"

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

// Database access.
val libsDatabase = Seq(
  "org.scalikejdbc" %% "scalikejdbc" % "4.0.0",
  "org.xerial" % "sqlite-jdbc" % "3.45.2.0",
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
  .in(file("apps/main"))
  .settings(
    commonSettings,
    name := "silver-brain",
    libraryDependencies ++= Seq(
      libCliArgsParser,
      libLoggerImplementation
    )
  )
  .dependsOn(silverBrainHttpServer, silverBrainSqliteStore)
  .aggregate(silverBrainHttpServer, silverBrainSqliteStore)
  .enablePlugins(JavaAppPackaging)

// ============================================================
//  Silver Brain Http Server
// ============================================================

lazy val silverBrainHttpServer = project
  .in(file("libs/http_server"))
  .settings(
    commonSettings,
    name := "silver-brain-http-server",
    libraryDependencies ++= Seq(
      libHttpServer,
      libLoggerInterface
    ) ++ libsJson ++ libsTestFramework
  )
  .dependsOn(silverBrainCore)
  .aggregate(silverBrainCore)

// ============================================================
//  Sqlite Store
// ============================================================

lazy val silverBrainSqliteStore = project
  .in(file("libs/sqlite_store"))
  .settings(
    commonSettings,
    name := "silver-brain-sqlite-store",
    libraryDependencies ++= Seq(
      libUniqueId,
      libOsLib,
      libLoggerInterface
    ) ++ libsDatabase ++ libsTestFramework
  )
  .dependsOn(silverBrainCore)
  .aggregate(silverBrainCore)

// ============================================================
//  Silver Brain Core
// ============================================================

lazy val silverBrainCore =
  project
    .in(file("libs/core"))
    .settings(
      commonSettings,
      name := "silver-brain-core",
      libraryDependencies ++= Seq(
        libParserCombinator
      ) ++ libsTestFramework
    )
