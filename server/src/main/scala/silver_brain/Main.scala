package silver_brain

import cask.main.Routes
import silver_brain.common._
import silver_brain.http.ConceptMapRoutes
import silver_brain.http.HelloRoutes
import org.flywaydb.core.Flyway
import scopt.OParser

import http.HttpServer

@main def run(args: String*): Unit = {
  val flyway = Flyway
    .configure()
    .dataSource(
      "jdbc:sqlite:/home/sheep/temp/silver-brain/a.sqlite",
      null,
      null
    )
    .load()

  flyway.migrate()
}

def main(args: String*): Unit = {
  try {
    given config: AppConfig = generateAppConfig(args)

    given DatabaseConfig = config.database
    given storeConnector: StoreConnector = SqliteStoreConnector()
    given conceptMapStore: concept_map.SqlStore = concept_map.SqlStore()
    given conceptMapService: concept_map.Service = concept_map.Service()

    val routes = Seq(
      HelloRoutes(),
      ConceptMapRoutes()
    )

    val server = HttpServer(routes)
    server.start()
  } catch { case _: Exception => () }
}

def generateAppConfig(args: Seq[String]): AppConfig = {
  val parser = CommandLineArgsParser()
  parser.parse(args) match {
    case None => {
      println("Invalid argument")
      throw new Exception()
    }
    case Some(cmdArgs) => {
      AppConfig(
        server = ServerConfig(
          port = firstSome(cmdArgs.port, Some(8080))
        ),
        database = DatabaseConfig(
          rootDir = firstSome(Some(os.home / "temp" / "silver-brain")),
          defaultDatabaseName = "silver-brain"
        )
      )
    }
  }
}

def firstSome[A](values: Option[A]*): A = {
  values.find(it => it.isDefined).get.get
}
