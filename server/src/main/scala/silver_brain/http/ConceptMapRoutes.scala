package silver_brain
package http

import cask._

import common._

case class CreateConceptRequest(
    name: String,
    contentType: Option[String],
    content: Option[String]
)

class ConceptMapRoutes(
    conceptMapService: concept_map.Service,
    defaultDatabaseName: String
) extends MainRoutes {
  @get("/concepts/:uuid")
  def getConceptByUuid(
      uuid: String,
      conceptProps: String = "",
      linkLevel: Int = 0,
      linkedConceptProps: String = "",
      request: Request
  ): Response[String] = {
    given DatabaseName = request.databaseNameOrDefault(defaultDatabaseName)

    conceptMapService
      .getConceptByUuid(
        uuid,
        conceptProps.commaSeparatedTokens,
        linkLevel,
        linkedConceptProps.commaSeparatedTokens
      )
      .toWebResponse()
  }

  @get("/concepts")
  def searchConcept(
      search: String,
      conceptProps: String = "",
      linkLevel: Int = 0,
      linkedConceptProps: String = "",
      request: Request
  ): Response[String] = {
    given DatabaseName = request.databaseNameOrDefault(defaultDatabaseName)

    conceptMapService
      .searchConcept(
        search,
        conceptProps.commaSeparatedTokens,
        linkLevel,
        linkedConceptProps.commaSeparatedTokens
      )
      .toWebResponse()
  }
  @post("/concepts")
  def createConcept(
      request: Request
  ): Response[String] = {
    given DatabaseName = request.databaseNameOrDefault(defaultDatabaseName)

    (for
      json <- request.parseJsonBody[CreateConceptRequest]()
      result <- conceptMapService.createConcept(
        json.name,
        json.contentType,
        json.content
      )
    yield result).toWebResponse(201)
  }

  case class UpdateConceptRequest(
      name: Option[String],
      contentType: Option[String],
      content: Option[String]
  )

  @patch("/concepts/:uuid")
  def updateConcept(uuid: String, request: Request): Response[String] = {
    given DatabaseName = request.databaseNameOrDefault(defaultDatabaseName)

    (for
      json <- request.parseJsonBody[UpdateConceptRequest]()
      result <- conceptMapService
        .updateConcept(
          uuid,
          json.name,
          json.contentType,
          json.content
        )
    yield result).toWebResponse()
  }

  initialize()
}
