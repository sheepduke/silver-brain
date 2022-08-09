package silver_brain
package migration

import com.github.nscala_time.time.Imports._
import silver_brain.common._
import silver_brain.concept_map.ConceptLink
import scalikejdbc._
import utest._

case class ConceptV2(
    uuid: String,
    name: String,
    contentType: String,
    content: String,
    createTime: String,
    updateTime: String
)

object ConceptV2 {
  def selectAll()(using
      storeConnector: StoreConnector
  )(using DatabaseName): Seq[ConceptV2] = {
    storeConnector.withReadOnly { session =>
      given DBSession = session

      sql"select * from concept"
        .map(rs =>
          ConceptV2(
            uuid = rs.string(1),
            name = rs.string(2),
            contentType = rs.string(3),
            content = rs.string(4),
            createTime = rs.string(5),
            updateTime = rs.string(6)
          )
        )
        .list
        .apply()
    }
  }
}

extension (concept: ConceptV2) {
  def shouldBe(other: ConceptV1): Unit = {
    concept.uuid ==> other.uuid
    concept.name ==> other.name
    concept.contentType ==> other.contentType
    concept.content ==> other.content

    concept.createTime.toDateTime.isEqual(
      other.createTime.replace(" ", "T").toDateTime
    )

    concept.updateTime.toDateTime.isEqual(
      other.updateTime.replace(" ", "T").toDateTime
    )
  }
}

case class ConceptLinkV2(
    uuid: String,
    source: String,
    relation: String,
    target: String,
    isMutual: Boolean,
    createTime: String,
    updateTime: String
)

object ConceptLinkV2 {
  def selectAll()(using storeConnector: StoreConnector)(using DatabaseName) = {
    storeConnector.withReadOnly { session =>
      given DBSession = session

      sql"select * from concept_link"
        .map(rs =>
          ConceptLinkV2(
            uuid = rs.string(1),
            source = rs.string(2),
            relation = rs.string(3),
            target = rs.string(4),
            isMutual = rs.boolean(5),
            createTime = rs.string(6),
            updateTime = rs.string(7)
          )
        )
        .list
        .apply()
    }
  }
}
