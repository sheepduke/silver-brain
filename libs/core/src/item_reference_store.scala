package silver_brain.core

case class CreateItemReferenceArgs(
    source: String,
    target: String,
    annotation: String
)

case class UpdateItemReferenceArgs(
    id: String,
    annotation: String
)

trait ItemReferenceStore:
  def getReference(id: String): StoreResult[Reference]

  def getReferences(ids: Seq[String]): StoreResult[Seq[Reference]]

  def createReference(
      source: String,
      target: String,
      annotation: String
  ): StoreResult[String]

  def updateReference(id: String, annotation: String): StoreResult[Unit]

  def deleteReference(id: String): StoreResult[Unit]
