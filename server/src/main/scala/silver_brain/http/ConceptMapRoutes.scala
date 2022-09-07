package silver_brain.http

import cask._
import silver_brain.common._
import silver_brain.concept_map.Service

case class CreateConceptRequest(
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None
)

case class UpdateConceptRequest(
    name: Option[String] = None,
    contentType: Option[String] = None,
    content: Option[String] = None
)

class ConceptMapRoutes(
    conceptMapService: Service,
    defaultDatabaseName: String
) extends MainRoutes {
  @get("/concepts/:uuid")
  def getConcept(
      uuid: String,
      conceptProps: String = "",
      linkLevel: Int = 0,
      linkedConceptProps: String = "",
      request: Request
  ): Response[String] = {
    given DatabaseName = request.databaseNameOrDefault(defaultDatabaseName)

    conceptMapService
      .getConcept(
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
      .searchConcepts(
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

  @patch("/concepts/:uuid")
  def updateConcept(uuid: String, request: Request): Response[String] = {
    given DatabaseName = request.databaseNameOrDefault(defaultDatabaseName)

    println(request.text())

    (for
      json <- request.parseJsonBody[UpdateConceptRequest]()
      _ <- conceptMapService
        .updateConcept(
          uuid,
          json.name,
          json.contentType,
          json.content
        )
    yield "").toWebResponse(204)
  }

  initialize()
}

// import silver_brain.http._

// requests.post(
//   "http://localhost:8080/concepts",
//   data = write(CreateConceptRequest(name = "Snow", contentType = Some("text/org")))).data.toString

// requests.get("http://localhost:8080/concepts?search=").data.toString
// requests.patch("http://localhost:8080/concepts/ece15b78-c507-4baf-ba4b-6c3cf7557547", data = write(UpdateConceptRequest(name = Some("Danny"))))
