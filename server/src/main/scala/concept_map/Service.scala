package com.sheepduke.silver_brain
package concept_map

import common._

class Service(using store: Store) {
  def getConceptByUuid(
      uuid: String,
      properties: String
  )(using dbName: DatabaseName): ServiceResponse[Concept] = {
    val result =
      store.getConceptByUuid(uuid, selectContent = true, selectTime = false)
    result match {
      case None          => Left(ServiceError.NotFound)
      case Some(concept) => Right(concept)
    }
  }
}
