package silver_brain.concept_map

import com.github.nscala_time.time.Imports._
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
}
