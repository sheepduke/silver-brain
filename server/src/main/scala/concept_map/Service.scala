package com.sheepduke.silver_brain
package concept_map

import javax.management.ServiceNotFoundException
import scala.collection.mutable.ListBuffer

import common._

class Service(using store: Store) {
  def getConceptByUuid(
      uuid: String,
      properties: Seq[String] = Seq(),
      loadLinkLevel: Int = 0,
      linkedProperties: Seq[String] = Seq()
  )(using dbName: DatabaseName): ServiceResponse[Concept] = {

    for
      conceptProperties <- properties.toConceptProperties
      linkedConceptProperties <- linkedProperties.toConceptProperties
      concept <- store
        .getConceptByUuid(
          uuid,
          conceptProperties,
          loadLinkLevel,
          linkedConceptProperties
        )
        .toRight(NotFoundError())
    yield concept
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
