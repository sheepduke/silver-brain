package com.sheepduke.silver_brain
package concept_map

import common._
import scalikejdbc._
import com.github.nscala_time.time.Imports._

trait Store {
  def getConceptByUuid(
      uuid: String,
      selectContent: Boolean = false,
      selectTime: Boolean = false
  )(using dbName: DatabaseName): Option[Concept]
}

class SqlStore(using storeConnector: StoreConnector) extends Store {
  def getConceptByUuid(
      uuid: String,
      selectContent: Boolean = false,
      selectTime: Boolean = false
  )(using dbName: DatabaseName): Option[Concept] = {
    storeConnector.withTransaction { session =>
      given DBSession = session

      val contentSql =
        if selectContent then sqls",content_type,content" else sqls""

      val timeSql =
        if selectTime then sqls",create_time,update_time" else sqls""

      val concept =
        sql"select uuid, name $contentSql $timeSql from concept where uuid = $uuid"
          .map(_.dataRowToConcept(selectContent, selectTime))
          .single
          .apply()

      concept
    }
  }
}

extension (rs: WrappedResultSet) {
  def dataRowToConcept(selectContent: Boolean, selectTime: Boolean): Concept = {
    Concept(
      uuid = rs.string("uuid"),
      name = rs.string("name"),
      contentType =
        if selectContent then Some(rs.string("content_type")) else None,
      content = if selectContent then Some(rs.string("content")) else None,
      createTime =
        if selectTime then Some(rs.string("create_time").toDateTime) else None,
      updateTime =
        if selectTime then Some(rs.string("update_time").toDateTime) else None
    )
  }
}

// import common._
// import concept_map._
// val config = AppConfig()
// given Store = SqliteStore(config.database.rootDir)

// SqlStore("a")
// res0.getConceptByUuid(
//   "8c560a8e-e9a4-4f14-9aa0-db1f13ec4cd8",
//   selectContent = true,
//   selectTime = true)

// Js.Obj(
//   "contentType" -> res8.contentType,
//   "content" -> res8.content
// )

// res3.get
