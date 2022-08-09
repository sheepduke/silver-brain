package silver_brain
package concept_map

import common._
import scalikejdbc.DBSession

import javax.management.ServiceNotFoundException
import scala.collection.mutable.ListBuffer

class Service(using store: Store)(using storeConnector: StoreConnector) {
  def getConceptByUuid(
      uuid: String,
      properties: Seq[String] = Seq(),
      loadLinkLevel: Int = 0,
      linkedProperties: Seq[String] = Seq()
  )(using DatabaseName): ServiceResponse[Concept] = {
    storeConnector.withTransaction { session =>
      given DBSession = session

      for
        conceptProps <- properties.toConceptProperties
        linkedConceptProps <- linkedProperties.toConceptProperties
        option = LoadConceptOption(
          conceptProps,
          loadLinkLevel,
          linkedConceptProps
        )
        conceptOpt <- store.getConceptByUuid(uuid)(using option)
        concept <- conceptOpt.toRight(NotFoundError())
      yield concept
    }
  }

  def searchConcept(
      search: String,
      properties: Seq[String] = Seq(),
      loadLinkLevel: Int = 0,
      linkedProperties: Seq[String] = Seq()
  )(using DatabaseName): ServiceResponse[Seq[Concept]] = {
    for
      conceptProps <- properties.toConceptProperties
      linkedConceptProps <- linkedProperties.toConceptProperties
      option = LoadConceptOption(
        conceptProps,
        loadLinkLevel,
        linkedConceptProps
      )
      concepts <- store.searchConcepts(search)(using option)
    yield concepts
  }

  def updateConcept(
      uuid: String,
      name: Option[String],
      contentType: Option[String],
      content: Option[String]
  )(using DatabaseName): ServiceResponse[Concept] = {
    Left(BadRequestError())
  }
}

extension (strings: Seq[String]) {
  def toConceptProperties: Either[ServiceError, Seq[ConceptProperty]] = {
    val iterator = strings.iterator
    val props = ListBuffer[ConceptProperty]()
    var error: Option[ServiceError] = None

    while iterator.hasNext && error.isEmpty
    do
      iterator.next.toConceptProperty match {
        case Right(prop) => props.addOne(prop)
        case Left(err)   => error = Some(err)
      }

    if error.isEmpty then Right(props.toSeq) else Left(error.get)
  }
}

extension (string: String) {
  def toConceptProperty: Either[ServiceError, ConceptProperty] = {
    string.toLowerCase match {
      case "content" => Right(ConceptProperty.Content)
      case "time"    => Right(ConceptProperty.Time)
      case _         => Left(BadRequestError(s"Invalid conceptProps '$string'"))
    }
  }
}
