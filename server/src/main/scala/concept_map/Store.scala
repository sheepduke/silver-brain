package com.sheepduke.silver_brain
package concept_map

import com.github.nscala_time.time.Imports._
import scalikejdbc._

import common._

enum ConceptProperty {
  case Content, Time
}

case class LoadConceptOption(
    conceptProps: Seq[ConceptProperty] = Seq(),
    loadLinkLevel: Int = 0,
    linkedConceptProps: Seq[ConceptProperty] = Seq()
)

trait Store {
  def getConceptByUuid(uuid: String)(using LoadConceptOption)(using
      DatabaseName
  ): Option[Concept]

  def searchConcept(search: String)(using LoadConceptOption)(using
      DatabaseName
  ): Seq[Concept]
}

class SqlStore(using storeConnector: StoreConnector) extends Store {
  def getConceptByUuid(uuid: String)(using LoadConceptOption)(using
      DatabaseName
  ): Option[Concept] = {
    storeConnector.withTransaction { session =>
      given DBSession = session

      loadConceptByUuid(uuid)
    }
  }

  def searchConcept(
      search: String
  )(using LoadConceptOption)(using DatabaseName): Seq[Concept] = {
    storeConnector.withTransaction { session =>
      given DBSession = session

      val searchString = s"%$search%"
      sql"select uuid from concept where name like $searchString"
        .map(_.string("uuid"))
        .list
        .apply()
        .map(loadConceptByUuid(_))
        .filter(_.nonEmpty)
        .map(_.get)
    }
  }

  private def loadConceptByUuid(
      uuid: String
  )(using
      option: LoadConceptOption
  )(using DatabaseName)(using DBSession): Option[Concept] = {
    val loadContent = option.conceptProps.contains(ConceptProperty.Content)
    val loadTime = option.conceptProps.contains(ConceptProperty.Time)

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
      if option.loadLinkLevel > 0 then
        loadConceptLinks(c)(using
          option.copy(loadLinkLevel = option.loadLinkLevel - 1)
        )
      else c
  }

  private def loadConceptLinks(concept: Concept)(using
      option: LoadConceptOption
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
        loadConceptByUuid(uuid).get
      )
    }

    links.foreach { (sourceUuid, relationUuid, targetUuid, isMutual) =>
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
      inboundLinks = Some(inboundLinks.toSeq),
      outboundLinks = Some(outboundLinks.toSeq),
      mutualLinks = Some(mutualLinks.toSeq)
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
