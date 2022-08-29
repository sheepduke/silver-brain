package silver_brain

import cask.main.Routes
import org.flywaydb.core.Flyway
import os.FilePath
import scopt.OParser
import silver_brain.common._
import silver_brain.http.ConceptMapRoutes
import silver_brain.http.HelloRoutes
import silver_brain.http.HttpServer

@main def main(args: String*): Unit = {
  try {
    val globalSettings = generateAppConfig(args)
    startServer(globalSettings)
  } catch {
    case _: Exception => ()
  }
}

def startServer(globalSettings: GlobalSettings): HttpServer = {
  val storeConnector = SqliteStoreConnector(
    globalSettings.database.rootDir
  )
  val conceptMapStore = concept_map.SqlStore(storeConnector)
  val conceptMapService = concept_map.Service(conceptMapStore)

  // Run migrations.
  globalSettings.database.rootDir / "a.sqlite"

  val routes = Seq(
    HelloRoutes(),
    ConceptMapRoutes(conceptMapService, globalSettings.database.defaultDbName)
  )

  val server = HttpServer(routes, globalSettings.server.port)
  server.start()
  server
}

def runMigrations(sqliteFile: FilePath): Unit = {
  val flyway = Flyway
    .configure()
    .dataSource(
      s"jdbc:sqlite:$sqliteFile",
      null,
      null
    )
    .load()

  flyway.migrate()
}

def generateAppConfig(args: Seq[String]): GlobalSettings = {
  val parser = CommandLineArgsParser()
  parser.parse(args) match {
    case None => {
      println("Invalid argument")
      throw new Exception()
    }
    case Some(cmdArgs) => {
      GlobalSettings(
        server = ServerSettings(
          port = firstSome(cmdArgs.port, Some(8080))
        ),
        database = DatabaseSettings(
          rootDir = firstSome(Some(os.home / "temp" / "silver-brain")),
          defaultDbName = "silver-brain"
        )
      )
    }
  }
}

def firstSome[A](values: Option[A]*): A = {
  values.find(it => it.isDefined).get.get
}
