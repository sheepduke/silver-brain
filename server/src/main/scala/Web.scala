import cask._
import common.SqliteStoreSession
import concept_map.SqlStore
import concept_map.Concept

object WebApplication extends MainRoutes {
  val storeSession =
    SqliteStoreSession("/home/sheep/temp/silver-brain", List("a"))

  @get("/")
  def hello() = {
    "Hello, world"
  }

  @get("/api/concepts/:uuid")
  def getConceptByUuid(uuid: String): Response[String] = {
    val store = SqlStore(storeSession, "a")

    val maybeConcept = store.getConceptByUuid(uuid)

    maybeConcept match {
      case Some(concept) =>
        Response(
          ujson.write(Map("uuid" -> concept.uuid, "name" -> concept.name))
        )
      case None => Abort(404)
    }
  }

  initialize()
}
