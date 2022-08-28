package silver_brain
package concept_map

import com.github.nscala_time.time.Imports._

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
  def getConceptByUuid(uuid: String, loadOption: LoadConceptOption)(using
      DatabaseName
  ): DatabaseResponse[Option[Concept]]

  def searchConcepts(search: String, loadOption: LoadConceptOption)(using
      DatabaseName
  ): DatabaseResponse[Seq[Concept]]

  def updateConcept(
      uuid: String,
      name: Option[String],
      contentType: Option[String],
      content: Option[String]
  ): DatabaseResponse[Concept]
}
