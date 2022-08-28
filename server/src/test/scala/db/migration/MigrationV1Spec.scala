package db.migration

import com.github.nscala_time.time.Imports._
import db.common._
import db.model.v1 as model
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import scalikejdbc._
import scalikejdbc.scalatest.AutoRollback

class MigrationV1Spec extends FixtureAnyFlatSpec with AutoRollback {
  override def db() = InMemorySqliteDb.create()

  override def fixture(using DBSession) = {}

  behavior of "Empty database"

  it should "migrate successfully" in { implicit session =>
    MigrationRunner.run("1")

    assertResult(0)(model.Concept.countAll())
    assertResult(0)(model.ConceptRelation.countAll())
  }

  behavior of "Rows inserted after migration"

  it should "be read properly" in { implicit session =>
    MigrationRunner.run("1")

    TestDataManager.insertV1ConceptRows()
    TestDataManager.insertV1ConceptRelationRows()

    assertResult(3)(model.Concept.countAll())

    val concept = model.Concept.find(1).get
    assertResult("1")(concept.uuid)
    assertResult("Father")(concept.name)
    assertResult("2022-08-26T14:30:54.289089Z".toDateTime)(concept.updatedAt)

    assertResult(4)(model.ConceptRelation.countAll())

    val relation = model.ConceptRelation.find(4).get
    assertResult("2")(relation.source)
    assertResult("3")(relation.target)
  }
}
