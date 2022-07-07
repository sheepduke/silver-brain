package com.sheepduke.silver_brain

import cask._
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization as json
import common.{DatabaseName, ServiceError}

object WebApplication extends MainRoutes {
  import AppContext.given

  val databaseNameHeader = "X-Database"

  @get("/")
  def hello() = {
    "Hello, world"
  }

  @get("/api/concepts/:uuid")
  def getConceptByUuid(uuid: String, request: Request): Response[String] = {
    given DatabaseName = request.databaseName

    concept_map.Service().getConceptByUuid(uuid, "13242").toWebResponse()
  }

  initialize()
}

extension (request: Request) {
  def databaseName: String = {
    request.headers.get("X-Database") match {
      case Some(list) => list.toString
      case None       => AppContext.config.database.defaultDatabaseName
    }
  }
}

extension [A](response: common.ServiceResponse[A]) {
  def toWebResponse(): Response[String] = {
    given formats: Formats = Serialization.formats(NoTypeHints)

    response match {
      case Right(value)                  => Response(json.write(value))
      case Left(ServiceError.BadRequest) => Abort(400)
      case Left(ServiceError.NotFound)   => Abort(404)
    }
  }
}

object AppContext {
  import common._

  given config: AppConfig = AppConfig()
  given DatabaseConfig = config.database

  given storeConnector: StoreConnector = SqliteStoreConnector()

  given concept_map.SqlStore = concept_map.SqlStore()
}
