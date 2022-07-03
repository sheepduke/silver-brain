ThisBuild / scalaVersion := "3.1.2"
ThisBuild / organization := "com.sheepduke"

libraryDependencies ++= Seq(
  // Utility.
  "com.lihaoyi" %% "upickle" % "2.0.0",
  "com.github.nscala-time" %% "nscala-time" % "2.30.0",

  // Database access.
  "org.scalikejdbc" %% "scalikejdbc" % "4.0.0",
  "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3",

  // Web framework.
  "com.lihaoyi" %% "cask" % "0.8.3"
)
