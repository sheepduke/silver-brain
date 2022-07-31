package com.sheepduke.silver_brain

// import cask._

import cask.main.Routes
import com.sheepduke.silver_brain.common._
import com.sheepduke.silver_brain.http.ConceptMapRoutes
import com.sheepduke.silver_brain.http.HelloRoutes
import scopt.OParser

import http._

@main def main(args: String*): Unit = {
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
