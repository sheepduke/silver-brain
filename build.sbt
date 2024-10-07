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

val libFastparse = "com.lihaoyi" %% "fastparse" % "3.1.0"

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
        libFastparse
      )
    )
