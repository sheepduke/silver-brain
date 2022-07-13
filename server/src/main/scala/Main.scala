package com.sheepduke.silver_brain

import cask._
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization as json

import common._

object WebApplication extends MainRoutes {
  import AppContext.given

  @get("/")
  def hello() = {
    "Hello, world"
  }

  @get("/api/concepts/:uuid")
  def getConceptByUuid(
      uuid: String,
      conceptProps: String = "",
      linkLevel: Int = 0,
      linkedConceptProps: String = "",
      request: Request
  ): Response[String] = {
    given DatabaseName = request.databaseName

    AppContext.conceptMapService
      .getConceptByUuid(
        uuid,
        conceptProps.commaSeparatedTokens,
        linkLevel,
        linkedConceptProps.commaSeparatedTokens
      )
      .toWebResponse()
  }

  initialize()
}

extension (request: Request) {
  def databaseName: String = {
    request.headers.get("x-database") match {
      case Some(list) => list.head
      case None       => AppContext.config.database.defaultDatabaseName
    }
  }
}

extension [A](response: common.ServiceResponse[A]) {
  def toWebResponse(): Response[String] = {
    given formats: Formats = DefaultFormats ++ JodaTimeSerializers.all

    response match {
      case Right(value)             => Response(json.write(value))
      case Left(BadRequestError(message)) => Response(message, 400)
      case Left(NotFoundError(message))   => Response(message, 404)
    }
  }
}

object AppContext {
  import common._

  given config: AppConfig = AppConfig()
  given DatabaseConfig = config.database

  given storeConnector: StoreConnector = SqliteStoreConnector()

  given conceptMapStore: concept_map.SqlStore = concept_map.SqlStore()

  given conceptMapService: concept_map.Service = concept_map.Service()
}
