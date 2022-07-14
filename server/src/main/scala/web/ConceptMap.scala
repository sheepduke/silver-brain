package com.sheepduke.silver_brain
package web

import cask._

import common._

case class ConceptMapRoutes() extends MainRoutes {
  import AppContext.given

  val context = AppContext

  @get("/concepts/:uuid")
  def getConceptByUuid(
      uuid: String,
      conceptProps: String = "",
      linkLevel: Int = 0,
      linkedConceptProps: String = "",
      request: Request
  ): Response[String] = {
    given DatabaseName = request.databaseName

    context.conceptMapService
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

    context.conceptMapService
      .searchConcept(
        search,
        conceptProps.commaSeparatedTokens,
        linkLevel,
        linkedConceptProps.commaSeparatedTokens
      )
      .toWebResponse
  }

  initialize()
}
