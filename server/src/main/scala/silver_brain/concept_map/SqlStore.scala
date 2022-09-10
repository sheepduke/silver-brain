package silver_brain.concept_map

import com.github.nscala_time.time.Imports._
import db.model.v2 as dao
import scalikejdbc._
import silver_brain.common._
import scala.collection.mutable
import scala.util.Try

class SqlStore(storeConnector: StoreConnector) extends Store {
  override def getConcept(uuid: String, loadOption: LoadConceptOption)(using
      DatabaseName
  ): Try[Concept] = {
    Try(storeConnector.withReadOnly { implicit session =>
      SqlStore.loadConceptByUuid(uuid, loadOption)
    })
  }

  override def searchConcepts(
      search: String,
      loadOption: LoadConceptOption
  )(using DatabaseName): Try[Seq[Concept]] = {
    Try(storeConnector.withReadOnly { implicit session =>
      val searchString = s"%$search%"

      dao.Concept
        .findAllBy(sqls"name like $searchString")
        .map(_.uuid)
        .map(SqlStore.loadConceptByUuid(_, loadOption))
    })
  }

  override def createConcept(
      name: String,
      contentType: String,
      content: String
  )(using DatabaseName): Try[String] = {
    Try(storeConnector.withTransaction { implicit session =>
      val uuid = Store.createUuid()
      val nowTime = DateTime.now()

      dao.Concept.create(uuid, name, contentType, content, nowTime, nowTime)
      uuid
    })
  }

  override def updateConcept(
      uuid: String,
      name: Option[String],
      contentType: Option[String],
      content: Option[String]
  )(using DatabaseName): Try[Unit] = {
    Try(storeConnector.withTransaction { implicit session =>
      dao.Concept.find(uuid) match {
        case None => throw ItemNotFoundException(uuid)
        case Some(concept) => {
          concept
            .copy(
              name = name.getOrElse(concept.name),
              contentType = contentType.getOrElse(concept.contentType),
              content = content.getOrElse(concept.content)
            )
            .save()
        }
      }
    })
  }
}

object SqlStore {
  private def loadConceptByUuid(
      uuid: String,
      loadOption: LoadConceptOption
  )(using DatabaseName)(using DBSession): Concept = {
    dao.Concept.find(uuid) match {
      case Some(conceptDao) =>
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
      case None => throw ItemNotFoundException(uuid)
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
        loadConceptByUuid(uuid, loadOption)
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
