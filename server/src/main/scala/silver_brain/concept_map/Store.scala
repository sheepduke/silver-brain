package silver_brain.concept_map

import com.github.nscala_time.time.Imports._
import db.model.v2 as dao
import silver_brain.common._

enum ConceptProperty {
  case Content, Time
}

case class LoadConceptOption(
    conceptProps: Seq[ConceptProperty] = Seq(),
    loadLinkLevel: Int = 0,
    linkedConceptProps: Seq[ConceptProperty] = Seq()
)

trait Store {
  def getConceptByUuid(uuid: String, loadOption: LoadConceptOption)(using
      DatabaseName
  ): ServiceResponse[Option[Concept]]

  def searchConcepts(search: String, loadOption: LoadConceptOption)(using
      DatabaseName
  ): ServiceResponse[Seq[Concept]]

  def createConcept(
      uuid: String,
      name: String,
      contentType: String,
      content: String,
      createTime: DateTime,
      updateTime: DateTime
  )(using DatabaseName): ServiceResponse[Concept]
}

extension (concept: dao.Concept) {
  def toCoreConcept(
      loadOption: LoadConceptOption = LoadConceptOption()
  ): Concept = {
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
