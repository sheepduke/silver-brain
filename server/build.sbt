ThisBuild / scalaVersion := "3.1.2"
ThisBuild / organization := "com.sheepduke"

// Library.
val jsonLib = "org.json4s" %% "json4s-native" % "4.0.5"
val timeLib = "com.github.nscala-time" %% "nscala-time" % "2.30.0"
val osLib = "com.lihaoyi" %% "os-lib" % "0.8.0"

val dbLibs = Seq(
  "org.scalikejdbc" %% "scalikejdbc" % "4.0.0",
  "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

val webLib = "com.lihaoyi" %% "cask" % "0.8.3"

libraryDependencies ++= Seq(
  jsonLib,
  timeLib,
  osLib,
  webLib
) ++ dbLibs
