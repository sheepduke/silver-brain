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

// Unique ID.
val libKsuid = "com.github.ksuid" % "ksuid" % "1.1.2"

// Parser combinator.
val libFastParse = "com.lihaoyi" %% "fastparse" % "3.1.1"

// OS interaction.
val libOsLib = "com.lihaoyi" %% "os-lib" % "0.11.1"

// Database access.
val libScalikeJdbc = "org.scalikejdbc" %% "scalikejdbc" % "4.0.0"
val libSqliteJdbc = "org.xerial" % "sqlite-jdbc" % "3.45.2.0"

// Database migration.
val libFlyway = "org.flywaydb" % "flyway-core" % "9.0.4"

// Logging.
val libSlf4j = "org.slf4j" % "slf4j-api" % "2.0.13"

// Test.
val libsScalaTest = Seq(
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test"
)

// ============================================================
//  Silver Brain
// ============================================================

lazy val silverBrain = (project in file("."))
  .settings(
    commonSettings,
    name := "Silver Brain",
    libraryDependencies ++= Seq(
    )
  )
  .aggregate(silverBrainSqliteStore)
  .dependsOn(silverBrainSqliteStore)

// ============================================================
//  Sqlite Store
// ============================================================

lazy val silverBrainSqliteStore = project
  .in(file("libs/sqlite_store"))
  .settings(
    commonSettings,
    name := "Silver Brain Sqlite Store",
    libraryDependencies ++= Seq(
      libKsuid,
      libOsLib,
      libScalikeJdbc,
      libSqliteJdbc,
      libFlyway,
      libSlf4j
    ) ++ libsScalaTest
  )
  .aggregate(silverBrainCore)
  .dependsOn(silverBrainCore)

// ============================================================
//  Silver Brain Core
// ============================================================

lazy val silverBrainCore =
  project
    .in(file("libs/core"))
    .settings(
      commonSettings,
      name := "Silver Brain Core",
      libraryDependencies ++= Seq(
        libFastParse
      ) ++ libsScalaTest
    )
