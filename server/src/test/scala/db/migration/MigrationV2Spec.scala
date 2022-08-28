package db.migration

import db.model.v2._
import org.scalatest.flatspec.FixtureAnyFlatSpec
import scalikejdbc._
import scalikejdbc.scalatest.AutoRollback

class MigrationV2Spec extends FixtureAnyFlatSpec with AutoRollback {
  override def db(): DB = tempSqliteDb()

  override def fixture(implicit session: DBSession): Unit = {}

  behavior of "Empty database"

  it should "migrate successfully" in { implicit session =>
    MigrationRunner.run("2")
  }

  behavior of "Rows inserted in V1 database"

  it should "be read normally" in { implicit session =>
    MigrationRunner.run("1")
    TestDataManager.insertV1ConceptRows()
    TestDataManager.insertV1ConceptRelationRows()
    MigrationRunner.run("1.5", "2")

    assertResult(5)(Concept.countAll())
    assertResult("Child")(Concept.find("3").get.name)

    assertResult(3)(ConceptLink.countAll())
    assertResult(false)(
      ConceptLink.findBy(sqls"source = ${"1"} and target = ${"3"}").get.isMutual
    )
    assertResult(false)(
      ConceptLink.findBy(sqls"source = ${"2"} and target = ${"3"}").get.isMutual
    )
    assertResult(true)(
      ConceptLink.findBy(sqls"source = ${"1"} and target = ${"2"}").get.isMutual
    )
  }
}
