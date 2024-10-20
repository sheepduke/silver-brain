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
//  Main
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
  .dependsOn(silverBrainSqliteRepo)
  .aggregate(silverBrainSqliteRepo)
  // .dependsOn(silverBrainHttpServer, silverBrainSqliteStore)
  // .aggregate(silverBrainHttpServer, silverBrainSqliteStore)
  .enablePlugins(JavaAppPackaging)

// ============================================================
//  Http Server
// ============================================================

// lazy val silverBrainHttpServer = project
//   .in(file("libs/http_server"))
//   .settings(
//     commonSettings,
//     name := "silver-brain-http-server",
//     libraryDependencies ++= Seq(
//       libHttpServer,
//       libLoggerInterface
//     ) ++ libsJson ++ libsTestFramework
//   )
//   .dependsOn(silverBrainStore)
//   .aggregate(silverBrainStore)

// ============================================================
//  Domain
// ============================================================

lazy val silverBrainLocalStore = project
  .in(file("libs/store.local"))
  .settings(
    commonSettings,
    name := "silver-brain-local-store",
    libraryDependencies ++= Seq(
    )
  )
  .dependsOn(silverBrainRepo)
  .aggregate(silverBrainRepo)

// ============================================================
//  Repo
// ============================================================

lazy val silverBrainRepo = project
  .in(file("libs/repo"))
  .settings(
    commonSettings,
    name := "silver-brain-repo",
    libraryDependencies ++= Seq(
      libUniqueId
    )
  )
  .dependsOn(silverBrainCore)
  .aggregate(silverBrainCore)

lazy val silverBrainSqliteRepo = project
  .in(file("libs/repo.sqlite"))
  .settings(
    commonSettings,
    name := "silver-brain-sqlite-repo",
    libraryDependencies ++= Seq(
      libUniqueId,
      libOsLib,
      libLoggerInterface
    ) ++ libsDatabase ++ libsTestFramework
  )
  .dependsOn(silverBrainRepo)
  .aggregate(silverBrainRepo)

// ============================================================
//  Core
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
