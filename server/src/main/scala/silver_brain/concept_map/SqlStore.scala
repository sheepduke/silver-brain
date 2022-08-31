package silver_brain.concept_map

import com.github.nscala_time.time.Imports._
import db.model.v2 as dao
import scalikejdbc._
import silver_brain.common._

import scala.collection.mutable
import scala.util.Try

class SqlStore(storeConnector: StoreConnector) extends Store {
  override def getConceptByUuid(uuid: String, loadOption: LoadConceptOption)(
      using DatabaseName
  ): ServiceResponse[Option[Concept]] = {
    storeConnector.withReadOnly { implicit session =>
      Try(SqlStore.loadConceptByUuid(uuid, loadOption)).toServiceResponse
    }
  }

  override def searchConcepts(
      search: String,
      loadOption: LoadConceptOption
  )(using DatabaseName): ServiceResponse[Seq[Concept]] = {
    Try(storeConnector.withReadOnly { implicit session =>
      val searchString = s"%$search%"

      dao.Concept
        .findAllBy(sqls"name like $searchString")
        .map(_.uuid)
        .map(SqlStore.loadConceptByUuid(_, loadOption))
        .map(_.get)
    }).toServiceResponse
  }

  override def createConcept(
      uuid: String,
      name: String,
      contentType: String,
      content: String,
      createTime: DateTime,
      updateTime: DateTime
  )(using DatabaseName): ServiceResponse[Concept] = {
    Try(storeConnector.withTransaction { implicit session =>
      dao.Concept
        .create(uuid, name, contentType, content, createTime, updateTime)
        .toCoreConcept()
    }).toServiceResponse
  }
}

object SqlStore {
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
