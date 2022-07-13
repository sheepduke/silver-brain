package com.sheepduke.silver_brain
package concept_map

import com.github.nscala_time.time.Imports._
import scalikejdbc._

import common._

enum ConceptProperty {
  case Content, Time
}

trait Store {
  def getConceptByUuid(
      uuid: String,
      conceptProperties: Seq[ConceptProperty],
      loadLinkLevel: Int,
      linkedConceptProperties: Seq[ConceptProperty]
  )(using DatabaseName): Option[Concept]
}

class SqlStore(using storeConnector: StoreConnector) extends Store {
  def getConceptByUuid(
      uuid: String,
      conceptProperties: Seq[ConceptProperty],
      loadLinkLevel: Int,
      linkedConceptProperties: Seq[ConceptProperty]
  )(using DatabaseName): Option[Concept] = {
    storeConnector.withTransaction { session =>
      given DBSession = session

      loadConceptByUuid(
        uuid,
        conceptProperties,
        loadLinkLevel,
        linkedConceptProperties
      )
    }
  }

  private def loadConceptByUuid(
      uuid: String,
      conceptProperties: Seq[ConceptProperty],
      loadLinkLevel: Int,
      linkedConceptProperties: Seq[ConceptProperty]
  )(using DatabaseName)(using DBSession) = {
    val loadContent = conceptProperties.contains(ConceptProperty.Content)
    val loadTime = conceptProperties.contains(ConceptProperty.Time)

    val contentSql =
      if loadContent then sqls",content_type,content" else sqls""

    val timeSql =
      if loadTime then sqls",create_time,update_time" else sqls""

    val concept =
      sql"select uuid, name $contentSql $timeSql from concept where uuid = $uuid"
        .map(_.dataRowToConcept(loadContent, loadTime))
        .single
        .apply()

    for c <- concept
    yield
      if loadLinkLevel > 0 then
        loadConceptLinks(c, linkedConceptProperties, loadLinkLevel - 1)
      else c
  }

  private def loadConceptLinks(
      concept: Concept,
      conceptProperties: Seq[ConceptProperty],
      loadLinkLevel: Int
  )(using DatabaseName)(using DBSession): Concept = {
    val uuid = concept.uuid

    val links = sql"""select source, relation, target, is_mutual
from concept_link
where source = $uuid or target = $uuid"""
      .map(rs =>
        (
          rs.string("source"),
          rs.string("relation"),
          rs.string("target"),
          rs.boolean("is_mutual")
        )
      )
      .list
      .apply()

    // Loop the fetched links and construct inbound, outbound and mutual links as lists.
    import collection.mutable.{HashMap, ListBuffer}

    val uuidConceptHashMap: HashMap[String, Concept] = HashMap()
    val inboundLinks: ListBuffer[ConceptInboundLink] = ListBuffer()
    val outboundLinks: ListBuffer[ConceptOutboundLink] = ListBuffer()
    val mutualLinks: ListBuffer[ConceptMutualLink] = ListBuffer()

    val getConcept = { (uuid: String) =>
      uuidConceptHashMap.getOrElseUpdate(
        uuid,
        loadConceptByUuid(
          uuid,
          conceptProperties,
          loadLinkLevel,
          conceptProperties
        ).get
      )
    }

    links.foreach { link =>
      val sourceUuid = link._1
      val relationUuid = link._2
      val targetUuid = link._3
      val isMutual = link._4

      if isMutual then
        mutualLinks.append(
          ConceptMutualLink(
            getConcept(relationUuid),
            getConcept(if uuid == sourceUuid then targetUuid else sourceUuid)
          )
        )
      else if uuid == targetUuid then
        inboundLinks.append(
          ConceptInboundLink(getConcept(sourceUuid), getConcept(relationUuid))
        )
      else
        outboundLinks.append(
          ConceptOutboundLink(getConcept(relationUuid), getConcept(targetUuid))
        )
    }

    // Construct result concept.
    concept.copy(
      inboundLinks = Some(inboundLinks.toList),
      outboundLinks = Some(outboundLinks.toList),
      mutualLinks = Some(mutualLinks.toList)
    )
  }
}

extension (rs: WrappedResultSet) {
  def dataRowToConcept(loadContent: Boolean, loadTime: Boolean): Concept = {
    Concept(
      uuid = rs.string("uuid"),
      name = rs.string("name"),
      contentType =
        if loadContent then Some(rs.string("content_type")) else None,
      content = if loadContent then Some(rs.string("content")) else None,
      createTime =
        if loadTime then Some(rs.string("create_time").toDateTime) else None,
      updateTime =
        if loadTime then Some(rs.string("update_time").toDateTime) else None
    )
  }
}
