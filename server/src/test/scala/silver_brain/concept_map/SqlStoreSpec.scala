package silver_brain.concept_map

import com.github.nscala_time.time.Imports._
import db.common._
import db.model.v2 as dao
import org.scalatest.flatspec.FixtureAnyFlatSpec
import scalikejdbc._
import scalikejdbc.scalatest.AutoRollback
import silver_brain.common._

class SqliteStoreSpec extends FixtureAnyFlatSpec with AutoRollback {
  override def db(): DB = InMemorySqliteDb.create()

  override def fixture(implicit session: DBSession): Unit = {}

  given DatabaseName = ""

  behavior of "getConceptByUuid"

  it should "return only uuid and name by default" in { implicit session =>
    val store = SqliteStoreSpec.createSqlStore(setupConcepts = true)

    val father =
      store.getConceptByUuid("1", LoadConceptOption()).getOrElse(None).get

    assertResult("1")(father.uuid)
    assertResult("Father")(father.name)
    assertResult(None)(father.contentType)
    assertResult(None)(father.content)
    assertResult(None)(father.createTime)
    assertResult(None)(father.updateTime)
    assertResult(None)(father.inboundLinks)
    assertResult(None)(father.outboundLinks)
    assertResult(None)(father.mutualLinks)
  }

  it should "return content when specified" in { implicit session =>
    val store = SqliteStoreSpec.createSqlStore(setupConcepts = true)

    val mother = store
      .getConceptByUuid(
        "2",
        LoadConceptOption(conceptProps =
          Seq(ConceptProperty.Content, ConceptProperty.Time)
        )
      )
      .getOrElse(None)
      .get

    assertResult("2")(mother.uuid)
    assertResult("Mother")(mother.name)
    assertResult(Some("text/md"))(mother.contentType)
    assertResult(Some(""))(mother.content)
    assert(mother.createTime.isDefined)
    assert(mother.updateTime.isDefined)
  }

  it should "return links of level 1" in { implicit session =>
    val store = SqliteStoreSpec.createSqlStore(
      setupConcepts = true,
      setupConceptLinks = true
    )

    val father =
      store
        .getConceptByUuid("1", LoadConceptOption(loadLinkLevel = 1))
        .getOrElse(None)
        .get

    assertResult(None)(father.content)
    assertResult(None)(father.createTime)
    assertResult(Some(Seq()))(father.inboundLinks)

    assertResult(1)(father.outboundLinks.get.size)
    val outboundLink = father.outboundLinks.get.head
    assertResult("fs")(outboundLink.relation.uuid)
    assertResult("FatherSon")(outboundLink.relation.name)
    assertResult(None)(outboundLink.relation.contentType)
    assertResult(None)(outboundLink.relation.createTime)
    assertResult("3")(outboundLink.target.uuid)
    assertResult(None)(outboundLink.target.contentType)
    assertResult(None)(outboundLink.target.createTime)

    assertResult(1)(father.mutualLinks.get.size)
    val mutualLink = father.mutualLinks.get.head
    assertResult("fm")(mutualLink.relation.uuid)
    assertResult("FatherMother")(mutualLink.relation.name)
    assertResult("2")(mutualLink.other.uuid)
  }

  it should "return links of level 1 with props" in { implicit session =>
    val store = SqliteStoreSpec.createSqlStore(
      setupConcepts = true,
      setupConceptLinks = true
    )

    val child = store
      .getConceptByUuid(
        "3",
        LoadConceptOption(
          conceptProps = Seq(ConceptProperty.Content),
          loadLinkLevel = 1,
          linkedConceptProps = Seq(ConceptProperty.Time)
        )
      )
      .getOrElse(None)
      .get

    assertResult(Some(""))(child.content)
    assertResult(None)(child.createTime)
    assertResult(None)(child.outboundLinks.get.head.target.content)
    assert(child.outboundLinks.get.head.target.createTime.isDefined)
    assertResult(None)(child.inboundLinks.get.head.source.outboundLinks)
  }

  it should "return links of level 2 with props" in { implicit session =>
    val store = SqliteStoreSpec.createSqlStore(
      setupConcepts = true,
      setupConceptLinks = true
    )

    val grandChild =
      store
        .getConceptByUuid(
          "4",
          LoadConceptOption(
            loadLinkLevel = 2,
            linkedConceptProps = Seq(ConceptProperty.Content)
          )
        )
        .getOrElse(None)
        .get

    val father =
      grandChild.inboundLinks.get.head.source.inboundLinks.get.head.source

    assertResult("Father")(father.name)
    assertResult(Some(""))(father.content)
    assertResult(None)(father.createTime)
  }
}

object SqliteStoreSpec {
  private def createSqlStore(
      setupConcepts: Boolean = false,
      setupConceptLinks: Boolean = false
  )(using session: DBSession) = {
    MigrationRunner.run()

    if setupConcepts then dao.Concept.batchInsert(TestDataManager.v2ConceptRows)

    if setupConceptLinks then
      dao.ConceptLink.batchInsert(TestDataManager.v2ConceptLinkRows)

    SqlStore(DummyStoreConnector(session))
  }
}
