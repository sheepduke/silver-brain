package silver_brain.concept_map

import com.github.nscala_time.time.Imports._
import db.model.v2 as dao
import silver_brain.common._
import java.util.UUID
import java.lang.Throwable
import scala.util.Try

enum ConceptProperty {
  case Content, Time
}

case class LoadConceptOption(
    conceptProps: Seq[ConceptProperty] = Seq(),
    loadLinkLevel: Int = 0,
    linkedConceptProps: Seq[ConceptProperty] = Seq()
)

case class UuidNotFoundException(uuid: String) extends Throwable

trait Store {
  def getConcept(uuid: String, loadOption: LoadConceptOption)(using
      DatabaseName
  ): Try[Option[Concept]]

  def searchConcepts(search: String, loadOption: LoadConceptOption)(using
      DatabaseName
  ): Try[Seq[Concept]]

  def createConcept(
      name: String,
      contentType: String,
      content: String
  )(using DatabaseName): Try[String]

  def updateConcept(
      uuid: String,
      name: Option[String],
      contentType: Option[String],
      content: Option[String]
  )(using DatabaseName): Try[Unit]
}

object Store {
  def createUuid(): String = {
    UUID.randomUUID().toString
  }
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
