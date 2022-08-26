// reStart command.
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")

// Flyway.
addSbtPlugin("io.github.davidmweber" % "flyway-sbt" % "7.4.0")

// ScalikeJDBC code generator.
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.36.0.3"
addSbtPlugin("org.scalikejdbc" %% "scalikejdbc-mapper-generator" % "4.0.0")

// Deploy uber JAR.
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.2.0")
