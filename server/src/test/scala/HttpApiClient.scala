package silver_brain

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.read
import org.json4s.native.Serialization.write
import requests.Response
import silver_brain.common.given_Formats
import silver_brain.concept_map.Concept
import silver_brain.http.CreateConceptRequest

class HttpApiClient(host: String = "localhost", port: Int) {
  extension (uri: String) {
    def expandUrl: String = {
      s"http://$host:$port$uri"
    }
  }

  extension (jsonString: String) {
    def toJson: JValue = {
      parse(jsonString)
    }
  }

  def hello(): Response = get("/")

  def getConcept(uuid: String): Response = get(s"/concepts/$uuid")

  def searchConcepts(search: String): Response = {
    get(
      s"/concepts?search=$search"
    )
  }

  def createConcept(request: CreateConceptRequest): Response = {
    post("/concepts", request)
  }

  def get(uri: String, dbName: Option[String] = None): Response = {
    requests.get(uri.expandUrl, check = false, headers = makeHeaders(dbName))
  }

  def post(
      uri: String,
      data: Any,
      dbName: Option[String] = None
  ): Response = {
    requests.post(
      uri.expandUrl,
      check = false,
      data = write(data),
      headers = makeHeaders(dbName)
    )
  }

  private def makeHeaders(dbName: Option[String] = None) = {
    dbName match {
      case Some(name) => Seq(("X-Database", name))
      case None       => Seq()
    }
  }
}

extension (response: Response) {
  def readContentAsString = response.data.toString

  def readContentAsConcept = read[Concept](response.readContentAsString)

  def readContentAsConceptSeq = read[Seq[Concept]](response.readContentAsString)

}
