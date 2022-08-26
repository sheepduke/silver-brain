ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "com.sheepduke"
Global / onChangedBuildSource := ReloadOnSourceChanges

// Library.
val jsonLibs = Seq(
  "org.json4s" %% "json4s-native" % "4.0.5",
  "org.json4s" %% "json4s-ext" % "4.0.5"
)

val timeLib = "com.github.nscala-time" %% "nscala-time" % "2.30.0"
val osLib = "com.lihaoyi" %% "os-lib" % "0.8.0"
val argLib = "com.github.scopt" %% "scopt" % "4.1.0"

val dbLibs = Seq(
  "org.scalikejdbc" %% "scalikejdbc" % "4.0.0",
  "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.flywaydb" % "flyway-core" % "9.0.4"
)

val httpServerLib = "com.lihaoyi" %% "cask" % "0.8.3"

val httpClientLib = "com.lihaoyi" %% "requests" % "0.7.0"

val testLibs = Seq(
  "com.lihaoyi" %% "utest" % "0.8.0" % "test"
)
testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies ++= Seq(
  timeLib,
  osLib,
  argLib,
  httpServerLib,
  httpClientLib
) ++ jsonLibs ++ dbLibs ++ testLibs

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

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

import scalikejdbc._

given config: AppConfig = AppConfig(
  server = ServerConfig(
    port = 8080
  ),
  database = DatabaseConfig(
    rootDir = os.home / "temp" / "silver-brain",
    defaultDatabaseName = "silver-brain"
  )
)

given DatabaseConfig = config.database
given storeConnector: StoreConnector = SqliteStoreConnector()
given conceptMapStore: concept_map.SqlStore = concept_map.SqlStore()
given conceptMapService: concept_map.Service = concept_map.Service()
"""
