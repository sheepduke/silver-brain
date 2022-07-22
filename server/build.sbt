ThisBuild / scalaVersion := "3.1.2"
ThisBuild / organization := "com.sheepduke"

// Library.
val jsonLibs = Seq(
  "org.json4s" %% "json4s-native" % "4.0.5",
  "org.json4s" %% "json4s-ext" % "4.0.5"
)

val timeLib = "com.github.nscala-time" %% "nscala-time" % "2.30.0"
val osLib = "com.lihaoyi" %% "os-lib" % "0.8.0"

val dbLibs = Seq(
  "org.scalikejdbc" %% "scalikejdbc" % "4.0.0",
  "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

val httpServerLib = "com.lihaoyi" %% "cask" % "0.8.3"

val httpClientLib = "com.lihaoyi" %% "requests" % "0.7.0"

libraryDependencies ++= Seq(
  timeLib,
  osLib,
  httpServerLib,
  httpClientLib
) ++ jsonLibs ++ dbLibs

// Flyway.
enablePlugins(FlywayPlugin)

flywayUrl := "jdbc:sqlite:/home/sheep/temp/silver-brain.sqlite"

// Initial commands.
console / initialCommands := """
import com.sheepduke.silver_brain._
import web.AppContext
import web.AppContext.given
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
"""
