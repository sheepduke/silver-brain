package com.sheepduke.silver_brain
package http

import cask._
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization._

import scala.util.Success
import scala.util.Try

import common._

given Formats = DefaultFormats ++ JodaTimeSerializers.all

extension (request: Request)(using config: AppConfig) {
  def databaseName: String = {
    request.headers.get("x-database") match {
      case Some(list) => list.head
      case None       => config.database.defaultDatabaseName
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
    response match {
      case Right(value)                   => Response(write(value))
      case Left(BadRequestError(message)) => Response(message, 400)
      case Left(NotFoundError(message))   => Response(message, 404)
      case Left(DatabaseError(message))   => Response(message, 500)
    }
  }
}

// case class AppContext()(using appConfig: AppConfig) {
//   import common._

//   given jsonFormats: Formats =

//   given config: AppConfig = appConfig
//   given DatabaseConfig = config.database

//   given storeConnector: StoreConnector = SqliteStoreConnector()

//   given conceptMapStore: concept_map.SqlStore = concept_map.SqlStore()

//   given conceptMapService: concept_map.Service = concept_map.Service()
// }
