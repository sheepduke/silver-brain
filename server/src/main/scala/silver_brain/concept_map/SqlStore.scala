package silver_brain.concept_map

import com.github.nscala_time.time.Imports._
import db.model.v2 as dao
import scalikejdbc._
import silver_brain.common._

import scala.collection.mutable
import scala.util.Try

class SqlStore(using storeConnector: StoreConnector) extends Store {
  def getConceptByUuid(uuid: String, loadOption: LoadConceptOption)(using
      DatabaseName
  ): DatabaseResponse[Option[Concept]] = {
    storeConnector.withReadOnly { implicit session =>
      Try(loadConceptByUuid(uuid, loadOption)).toDatabaseResponse
    }
  }

  def searchConcepts(
      search: String,
      loadOption: LoadConceptOption
  )(using DatabaseName): DatabaseResponse[Seq[Concept]] = {
    storeConnector.withReadOnly { implicit session =>
      val searchString = s"%$search%"

      Try(
        dao.Concept
          .findAllBy(sqls"name like $searchString")
          .map(_.uuid)
          .map(loadConceptByUuid(_, loadOption))
          .map(_.get)
      ).toDatabaseResponse
    }
  }

  def updateConcept(
      uuid: String,
      name: Option[String],
      contentType: Option[String],
      content: Option[String]
  ): Either[DatabaseError, Concept] = {
    Left(DatabaseError())
  }

  private def loadConceptByUuid(
      uuid: String,
      loadOption: LoadConceptOption
  )(using DatabaseName)(using DBSession): Option[Concept] = {
    dao.Concept
      .find(uuid)
      .map { conceptDao =>
        val concept = conceptDao.toCoreConcept(loadOption)

        if loadOption.loadLinkLevel > 0 then
          loadConceptLinks(
            concept,
            loadOption.copy(
              conceptProps = loadOption.linkedConceptProps,
              loadLinkLevel = loadOption.loadLinkLevel - 1
            )
          )
        else concept
      }
  }

  private def loadConceptLinks(concept: Concept, loadOption: LoadConceptOption)(
      using DatabaseName
  )(using DBSession): Concept = {
    val uuid = concept.uuid

    val links =
      dao.ConceptLink.findAllBy(sqls"source = $uuid or target = $uuid")

    // Loop the fetched links and construct inbound, outbound and mutual links as lists.
    import collection.mutable.{HashMap, ListBuffer}

    val uuidConceptHashMap: HashMap[String, Concept] = HashMap()
    val inboundLinks: ListBuffer[ConceptInboundLink] = ListBuffer()
    val outboundLinks: ListBuffer[ConceptOutboundLink] = ListBuffer()
    val mutualLinks: ListBuffer[ConceptMutualLink] = ListBuffer()

    val getConcept = { (uuid: String) =>
      uuidConceptHashMap.getOrElseUpdate(
        uuid,
        loadConceptByUuid(uuid, loadOption).get
      )
    }

    links.foreach { link =>
      if link.isMutual then
        mutualLinks.append(
          ConceptMutualLink(
            getConcept(link.relation),
            getConcept(if uuid == link.source then link.target else link.source)
          )
        )
      else if uuid == link.target then
        inboundLinks.append(
          ConceptInboundLink(getConcept(link.source), getConcept(link.relation))
        )
      else
        outboundLinks.append(
          ConceptOutboundLink(
            getConcept(link.relation),
            getConcept(link.target)
          )
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

extension (concept: dao.Concept) {
  def toCoreConcept(loadOption: LoadConceptOption): Concept = {
    val loadContent = loadOption.conceptProps.contains(ConceptProperty.Content)
    val loadTime = loadOption.conceptProps.contains(ConceptProperty.Time)

    Concept(
      uuid = concept.uuid,
      name = concept.name,
      contentType = if loadContent then Some(concept.contentType) else None,
      content = if loadContent then Some(concept.content) else None,
      createTime = if loadTime then Some(concept.createTime) else None,
      updateTime = if loadTime then Some(concept.updateTime) else None
    )
  }
}
