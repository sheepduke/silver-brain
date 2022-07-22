package com.sheepduke.silver_brain
package web

import cask._
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization._

import scala.util.Success
import scala.util.Try

import AppContext.given
import common._

extension (request: Request) {
  def databaseName: String = {
    request.headers.get("x-database") match {
      case Some(list) => list.head
      case None       => AppContext.config.database.defaultDatabaseName
    }
  }

  def parseJsonBody[A]()(using Manifest[A]): common.ServiceResponse[A] = {
    Try(read[A](request.text())).toEither.left
      .map(it =>
        BadRequestError("Failed to parse input JSON. " + it.getMessage)
      )
  }
}

extension [A](response: common.ServiceResponse[A]) {
  def toWebResponse: Response[String] = {
    given Formats = AppContext.jsonFormats

    response match {
      case Right(value)                   => Response(write(value))
      case Left(BadRequestError(message)) => Response(message, 400)
      case Left(NotFoundError(message))   => Response(message, 404)
      case Left(DatabaseError(message))   => Response(message, 500)
    }
  }
}

object AppContext {
  import common._

  given jsonFormats: Formats = DefaultFormats ++ JodaTimeSerializers.all

  given config: AppConfig = AppConfig()
  given DatabaseConfig = config.database

  given storeConnector: StoreConnector = SqliteStoreConnector()

  given conceptMapStore: concept_map.SqlStore = concept_map.SqlStore()

  given conceptMapService: concept_map.Service = concept_map.Service()
}
