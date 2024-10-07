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

val libFastParse = "com.lihaoyi" %% "fastparse" % "3.1.0"
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
      )
        ++ libsScalaTest
    )
