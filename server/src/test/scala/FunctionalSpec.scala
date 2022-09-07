package silver_brain

import io.undertow.Undertow
import org.scalactic.source.Position
import org.scalatest.BeforeAndAfter
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import silver_brain.common._
import silver_brain.http._

import java.net.ServerSocket
import scala.util.Try
import scala.util.Using

import common._

class FunctionalSpec
    extends AnyFeatureSpec
    with BeforeAndAfter
    with GivenWhenThen {

  private var server: HttpServer = null

  private val settings = GlobalSettings(
    server = ServerSettings(
      port = Using(ServerSocket(0))(_.getLocalPort()).get
    ),
    database = DatabaseSettings(
      rootDir = os.temp.dir(prefix = "silver-brain_"),
      defaultDbName = "silver-brain"
    )
  )

  private val httpClient = HttpApiClient(port = settings.server.port)

  // ----------------------------------------------------------------------

  before {
    silver_brain.runMigrations(
      settings.database.rootDir / s"${settings.database.defaultDbName}.sqlite"
    )

    server = silver_brain.startServer(settings)
  }

  after {
    server.stop()
  }

  // ----------------------------------------------------------------------

  Feature("GET /") {
    Scenario("Just return 'Hello, world' text") {
      When("called")
      val response = httpClient.hello()

      Then("return expected text")
      assertResult(200)(response.statusCode)
      assertResult("Hello, world")(response.readContentAsString)
    }
  }

  Feature("Get concepty by UUID") {
    Scenario("Database is empty") {
      When("called with any uuid")
      val response = httpClient.getConcept("1234")

      Then("return 404")
      assertResult(404)(response.statusCode)
    }
  }

  Feature("Create concept") {
    Scenario("Create concept with valid arguments") {
      Given("no existing concept")

      When("create a new concept")
      val response =
        httpClient.createConcept(
          CreateConceptRequest(
            name = "First Concept",
            contentType = Some("org/text")
          )
        )

      Then("return UUID of newly created concept")
      assertResult(201)(response.statusCode)
      val uuid = response.readContentAsString

      assertResult(36)(uuid.length)

      And("get concept uuid returns right value")
      val concept = httpClient.getConcept(uuid).readContentAsConcept

      assertResult(uuid)(concept.uuid)
      assertResult("First Concept")(concept.name)
    }
  }

  Feature("Update concept") {
    Scenario("Search concept and update it") {
      Given("concept name is 'First Concept'")
      val oldConcept =
        httpClient
          .searchConcepts("", "conceptProps=content,time")
          .readContentAsConceptSeq
          .head
      val uuid = oldConcept.uuid

      When("update the name to 'First Updated Concept'")
      httpClient.updateConcept(
        uuid,
        UpdateConceptRequest(
          name = Some("First Updated Concept"),
          content = Some("updated content")
        )
      )

      Then("get this concept returns updated value")
      val newConcept = httpClient
        .getConcept(uuid, "conceptProps=content,time")
        .readContentAsConcept

      assertResult("First Updated Concept")(newConcept.name)
      assertResult(oldConcept.contentType)(newConcept.contentType)
      assertResult("updated content")(newConcept.content.get)
    }
  }

  Feature("Search concept") {
    Scenario("Search all with empty search string") {
      Given("there is one concept created before")

      When("search with empty search string")
      val response = httpClient.searchConcepts("")

      Then("return a list of one concept")
      val concepts = response.readContentAsConceptSeq
      assertResult(1)(concepts.length)
      assertResult("First Concept")(concepts.head.name)
    }

    Scenario("Search with non-existing search string") {
      Given("there is one concept with name 'First Concept'")

      When("search with string 'invalid'")
      val response = httpClient.searchConcepts("invalid")

      Then("return empty list")
      assertResult("[]")(response.readContentAsString)
    }

    Scenario("Call with invalid db name in header") {
      When("search with non-existing database name 'a'")
      val response = httpClient.get("/concepts?search=", dbName = Some("a"))

      Then("return 404")
      assertResult(404)(response.statusCode)
    }
  }
}
