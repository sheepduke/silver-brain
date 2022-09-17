ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "com.sheepduke"
Global / onChangedBuildSource := ReloadOnSourceChanges

val dbLibs = Seq(
)

libraryDependencies ++= Seq(
  // JSON.
  "org.json4s" %% "json4s-native" % "4.0.5",
  "org.json4s" %% "json4s-ext" % "4.0.5",

  // Time.
  "com.github.nscala-time" %% "nscala-time" % "2.30.0",

  // OS interaction.
  "com.lihaoyi" %% "os-lib" % "0.8.0",

  // Argument parser.
  "com.github.scopt" %% "scopt" % "4.1.0",

  // HTTP server.
  "com.lihaoyi" %% "cask" % "0.8.3",

  // Database access.
  "org.scalikejdbc" %% "scalikejdbc" % "4.0.0",
  "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.flywaydb" % "flyway-core" % "9.0.4",

  // HTTP client.
  "com.lihaoyi" %% "requests" % "0.7.1",
  "com.softwaremill.sttp.client3" %% "core" % "3.7.6",

  // Test.
  "org.scalatest" %% "scalatest" % "3.2.13" % "test",
  "org.scalikejdbc" %% "scalikejdbc-test" % "4.0.0" % "test"
)

// Flyway.
enablePlugins(FlywayPlugin)

flywayUrl := "jdbc:sqlite:/home/sheep/temp/silver-brain/a.sqlite"
flywayLocations += "classpath:db/migration"

// ScalikeJDBC code generator.
enablePlugins(ScalikejdbcPlugin)

// Initial commands.
console / initialCommands := """
import silver_brain._
import silver_brain.common._
import silver_brain.concept_map._
import silver_brain.http._

import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.read
import org.json4s.native.Serialization.write

import scalikejdbc.{GlobalSettings as _, *}
import com.github.nscala_time.time.Imports._

val settings = GlobalSettings(
  server = ServerSettings(
    port = 8080
  ),
  database = DatabaseSettings(
    rootDir = os.home / "temp" / "silver-brain",
    defaultDbName = "silver-brain"
  )
)

val storeConnector = SqliteStoreConnector(settings.database.rootDir)
val conceptMapStore = concept_map.SqlStore(storeConnector)
val conceptMapService = concept_map.Service(conceptMapStore)

given Formats = DefaultFormats ++ JodaTimeSerializers.all
"""
