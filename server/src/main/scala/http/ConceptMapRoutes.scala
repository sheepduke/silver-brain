package com.sheepduke.silver_brain
package http

import cask._

import common._

case class ConceptMapRoutes()(using conceptMapService: concept_map.Service)(
    using
    AppConfig,
    DatabaseConfig
) extends MainRoutes {
  @get("/concepts/:uuid")
  def getConceptByUuid(
      uuid: String,
      conceptProps: String = "",
      linkLevel: Int = 0,
      linkedConceptProps: String = "",
      request: Request
  ): Response[String] = {
    given DatabaseName = request.databaseName

    conceptMapService
      .getConceptByUuid(
        uuid,
        conceptProps.commaSeparatedTokens,
        linkLevel,
        linkedConceptProps.commaSeparatedTokens
      )
      .toWebResponse
  }

  @get("/concepts")
  def searchConcept(
      search: String,
      conceptProps: String = "",
      linkLevel: Int = 0,
      linkedConceptProps: String = "",
      request: Request
  ): Response[String] = {
    given DatabaseName = request.databaseName

    conceptMapService
      .searchConcept(
        search,
        conceptProps.commaSeparatedTokens,
        linkLevel,
        linkedConceptProps.commaSeparatedTokens
      )
      .toWebResponse
  }

  case class UpdateConceptRequest(
      name: Option[String],
      contentType: Option[String],
      content: Option[String]
  )

  @patch("/concepts/:uuid")
  def updateConcept(uuid: String, request: Request): Response[String] = {
    given DatabaseName = request.databaseName

    val result = for
      json <- request.parseJsonBody[UpdateConceptRequest]()
      result <- conceptMapService
        .updateConcept(
          uuid,
          json.name,
          json.contentType,
          json.content
        )
    yield result

    result.toWebResponse
  }

  initialize()
}
