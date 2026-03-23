val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Silver Brain",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.github.ksuid" % "ksuid" % "1.1.2",
      "com.lihaoyi" %% "scalasql" % "0.1.9",
      "com.lihaoyi" %% "scalasql-simple" % "0.3.0",
      "org.xerial" % "sqlite-jdbc" % "3.45.3.0",
      "org.flywaydb" % "flyway-core" % "10.10.0",
      "org.scalameta" %% "munit" % "1.2.4" % Test
    )
  )
