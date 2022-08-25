package db.migration

import com.github.nscala_time.time.Imports._
import org.flywaydb.core.api.migration.BaseJavaMigration
import org.flywaydb.core.api.migration.Context
import scalikejdbc._

import java.sql.Connection
import java.util.UUID
import scala.collection.mutable
import scala.util.Using

class V2_0__Upgrade_schema extends BaseJavaMigration {
  case class ConceptLinkRow(
      source: String,
      target: String,
      createTime: String,
      updateTime: String
  )

  override def migrate(context: Context): Unit = {
    val db = DB(context.getConnection())
    db.autoClose(false)
    // migrate(context.getConnection())
    db.withinTx { implicit session =>
      migrate()
    }
  }

  def migrate()(using DBSession): Unit = {
    val relationCount = V2_0__Upgrade_schema.getRelationCount()

    V2_0__Upgrade_schema.migrateConceptTable()
    V2_0__Upgrade_schema.createConceptLinkTable()

    // if relationCount > 0 then {
    //   V2_0__Upgrade_schema.migrateConceptLinks()
    // }

    V2_0__Upgrade_schema.dropConceptRelationTable()
  }
}

object V2_0__Upgrade_schema {
  private def getRelationCount()(using DBSession): Int = {
    sql"select count(*) from concept_relation"
      .map(_.int(1))
      .single
      .apply()
      .get
  }

  private def migrateConceptTable()(using DBSession): Unit = {
    sql"alter table concept rename created_at to create_time".execute.apply()

    sql"alter table concept rename updated_at to update_time".execute.apply()

    sql"update concept set create_time = replace(create_time, ' ', 'T')".execute
      .apply()

    sql"update concept set update_time = replace(update_time, ' ', 'T')".execute
      .apply()
  }

  private def createConceptLinkTable()(using DBSession): Unit = {
    sql"""
create table concept_link(
  uuid text primary key,
  source text not null,
  relation text not null,
  target text not null,
  is_mutual boolean not null,
  create_time timestamp not null,
  update_time timestamp not null
)
""".execute.apply()
  }

  // private def migrateConceptLinks()(using connection: Connection): Unit = {
  //   val includesUuid = UUID.randomUUID().toString
  //   val includesName = "Includes"
  //   val relatesToUuid = UUID.randomUUID().toString
  //   val relatesToName = "Relates to"
  //   val now = DateTime.now().toString

  //   // Insert 2 relation concepts.
  //   insertConcept(includesUuid, includesName, "text/org", "", now, now)
  //   insertConcept(relatesToUuid, relatesToName, "text/org", "", now, now)

  //   // Map of source => target.
  //   val sourceTargetMap = mutable.HashMap[String, mutable.HashSet[String]]()

  //   // Map of source => row.
  //   val linkMap = mutable.HashMap[String, ConceptLinkRow]()

  //   val hasLinkTo: (String, String) => Boolean = { (from, to) =>
  //     sourceTargetMap.contains(from)
  //     && sourceTargetMap.get(from).get.contains(to)
  //   }

  //   // Iterate existing links and build up the 2 maps.
  //   Using.resource(connection.createStatement()) { statement =>
  //     val sql =
  //       "select source, target, created_at, updated_at from concept_relation"

  //     Using.resource(statement.executeQuery(sql)) { resultSet =>
  //       while resultSet.next()
  //       do {
  //         val source = resultSet.getString("source")
  //         val target = resultSet.getString("target")

  //         val link = ConceptLinkRow(
  //           source = source,
  //           target = target,
  //           createTime = resultSet.getString("created_at").replace(" ", "T"),
  //           updateTime = resultSet.getString("updated_at").replace(" ", "T")
  //         )

  //         if sourceTargetMap.contains(source) then
  //           sourceTargetMap.get(source).get.add(target)
  //         else sourceTargetMap.addOne(source, mutable.HashSet(target))

  //         linkMap.addOne(source, link)
  //       }
  //     }
  //   }

  //   // Iterate source => link map and insert the links.
  //   val sql = "insert into concept_link values (?,?,?,?,?,?,?)"
  //   Using.resource(connection.prepareStatement(sql)) { statement =>
  //     for link <- linkMap.values
  //     do {
  //       val uuid = UUID.randomUUID().toString
  //       val isMutual = hasLinkTo(link.target, link.source)
  //       val relationUuid = if isMutual then relatesToUuid else includesUuid

  //       statement.setString(1, uuid)
  //       statement.setString(2, link.source)
  //       statement.setString(3, relationUuid)
  //       statement.setString(4, link.target)
  //       statement.setBoolean(5, isMutual)
  //       statement.setString(6, link.createTime)
  //       statement.setString(7, link.updateTime)

  //       statement.executeUpdate()
  //     }
  //   }
  // }

  // private def insertConcept(
  //     uuid: String,
  //     name: String,
  //     contentType: String,
  //     content: String,
  //     createTime: String,
  //     updateTime: String
  // )(using connection: Connection) = {
  //   val insertConceptSql =
  //     "insert into concept values(?, ?, ?, ?, ?, ?)"
  //   Using.resource(connection.prepareStatement(insertConceptSql)) { statement =>
  //     statement.setString(1, uuid)
  //     statement.setString(2, name)
  //     statement.setString(3, contentType)
  //     statement.setString(4, content)
  //     statement.setString(5, createTime)
  //     statement.setString(6, updateTime)

  //     statement.executeUpdate()
  //   }
  // }

  private def dropConceptRelationTable()(using DBSession): Unit = {
    sql"drop table concept_relation".execute.apply()
  }
}
