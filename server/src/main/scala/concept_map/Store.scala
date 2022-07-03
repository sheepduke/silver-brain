package concept_map

import common._
import scalikejdbc._

trait Store {
  def getConceptByUuid(uuid: String): Option[Concept]
}

class SqlStore(storeSession: StoreSession, databaseName: String) extends Store {
  def getConceptByUuid(uuid: String): Option[Concept] = {
    storeSession.withSession(
      databaseName,
      { implicit session =>
        sql"select * from concept where id = $uuid"
          .map(rs => Concept(uuid = uuid, name = rs.string("name")))
          .single
          .apply()
      }
    )
  }
}

// import concept_map._
// val store = SqlStore(SqliteStoreSession("/home/sheep/temp/silver-brain", List("a")), "a")
// store.getConceptByUuid("5baab06f-d70d-4405-8511-3032d12448b3")
