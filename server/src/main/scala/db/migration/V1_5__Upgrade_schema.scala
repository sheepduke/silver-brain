package db.migration

import com.github.nscala_time.time.Imports._
import db.model.v1 as v1
import db.model.v1_5 as v1_5
import org.flywaydb.core.api.migration.BaseJavaMigration
import org.flywaydb.core.api.migration.Context
import scalikejdbc._

import java.util.UUID

/** V1.5 is a special state during migration.
  *
  * It used to be the dev branch of Silver Brain software. However, due to the
  * change of technical stack (from Common Lisp to Scala) this version was
  * intended to be rebuilt.
  *
  * This migration class mimics the original dev branch status. The
  * corresponding models should not be used in business code.
  */
class V1_5__Upgrade_schema extends DbSessionMigration {
  override def migrate()(using DBSession): Unit = {
    V1_5__Upgrade_schema.createNewConceptTable()
    V1_5__Upgrade_schema.createConceptLinkTable()
    V1_5__Upgrade_schema.migrateConcepts()

    // If there is more than one old relations, do the following:
    // 1. Create special concepts "Contains" and "Relates".
    // 2. For old relations, create links using the 2 concepts created above.
    //    - Parent becomes "Contains".
    //    - Friend becomes "Relates". Sorted by UUID string.
    val relationCount = v1.ConceptRelation.countAll()
    if relationCount > 0 then V1_5__Upgrade_schema.migrateConceptRelation()
  }
}

object V1_5__Upgrade_schema {
  private def createNewConceptTable()(using DBSession): Unit = {
    sql"""
CREATE TABLE IF NOT EXISTS concept_new (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    content_type TEXT NOT NULL,
    content TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
)
""".execute.apply()
  }

  private def createConceptLinkTable()(using DBSession): Unit = {
    sql"""
CREATE TABLE IF NOT EXISTS concept_link (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    source TEXT NOT NULL,
    relation TEXT NOT NULL,
    target TEXT NOT NULL,
    directionalp BOOLEAN NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
)
""".execute.apply()
  }

  private def migrateConcepts()(using DBSession): Unit = {
    val concepts = v1.Concept.findAll()
    for c <- concepts
    do
      v1_5.Concept.create(
        id = c.uuid,
        name = c.name,
        contentType = c.contentFormat,
        content = c.content,
        createdAt = c.createdAt,
        updatedAt = c.updatedAt
      )
  }

  private def migrateConceptRelation()(using DBSession): Unit = {
    // Insert special concepts.
    val nowTime = DateTime.now()

    val containsUuid = UUID.randomUUID().toString
    v1_5.Concept.create(
      id = containsUuid,
      name = "Contains",
      contentType = "text/org",
      content = "",
      createdAt = nowTime,
      updatedAt = nowTime
    )

    val relatesUuid = UUID.randomUUID().toString
    v1_5.Concept.create(
      id = relatesUuid,
      name = "Relates",
      contentType = "text/org",
      content = "",
      createdAt = nowTime,
      updatedAt = nowTime
    )

    // Build concept link map.
    case class RecordTime(createdAt: DateTime, updatedAt: DateTime)

    type Link = (String, String)

    val relationMap: Map[Link, RecordTime] = v1.ConceptRelation
      .findAll()
      .map(relation =>
        (relation.source, relation.target) -> RecordTime(
          createdAt = relation.createdAt,
          updatedAt = relation.updatedAt
        )
      )
      .toMap

    def areFriends(source: String, target: String) = {
      relationMap.contains((source, target))
      && relationMap.contains((target, source))
    }

    // Iterate through existing links and insert new one.
    for
      ((source, target), record) <- relationMap
      isDirectional = !areFriends(source, target)
    do
      if !isDirectional && source > target then ()
      else {
        v1_5.ConceptLink.create(
          id = UUID.randomUUID().toString,
          source = source,
          relation = if isDirectional then containsUuid else relatesUuid,
          target = target,
          directionalp = isDirectional,
          createdAt = record.createdAt,
          updatedAt = record.updatedAt
        )
      }
  }
}
