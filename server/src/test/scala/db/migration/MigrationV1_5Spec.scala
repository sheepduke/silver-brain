package db.migration

import com.github.nscala_time.time.Imports._
import db.model.v1_5._
import org.scalatest.flatspec.FixtureAnyFlatSpec
import scalikejdbc._
import scalikejdbc.scalatest.AutoRollback

class MigrationV1_5Spec extends FixtureAnyFlatSpec with AutoRollback {
  override def db() = tempSqliteDb()

  override def fixture(using DBSession) = {}

  behavior of "Empty database"

  it should "migrate successfully" in { implicit session =>
    MigrationRunner.run("1.5")

    assertResult(0)(Concept.countAll())
    assertResult(0)(ConceptLink.countAll())
  }

  behavior of "Database with V1 concepts and no links"

  it should "keep existing concepts but no special ones" in {
    implicit session =>
      MigrationRunner.runSingle("1")
      TestDataManager.insertV1ConceptRows()
      MigrationRunner.runSingle("1.5")

      assertResult(3)(Concept.countAll())

      val concept = Concept.find("3").get
      assertResult("Child")(concept.name)
      assertResult("2022-08-26T14:24:55.188886Z".toDateTime)(concept.createdAt)

      assertResult(0)(ConceptLink.countAll())
  }

  behavior of "Database with V1 concepts and relations"

  it should "migrate existing concepts and relations" in { implicit session =>
    MigrationRunner.runSingle("1")
    TestDataManager.insertV1ConceptRows()
    TestDataManager.insertV1ConceptRelationRows()
    MigrationRunner.runSingle("1.5")

    assertResult(5)(Concept.countAll())
    assertResult("Child")(Concept.find("3").get.name)

    val containsConcept = Concept.findBy(sqls"name = ${"Contains"}").get
    val relatesConcept = Concept.findBy(sqls"name = ${"Relates"}").get

    assertResult(3)(ConceptLink.countAll())

    val fatherMotherLink =
      ConceptLink.findBy(sqls"source = ${"1"} and target = ${"2"}").get
    assertResult(false)(fatherMotherLink.directionalp)
    assertResult("2022-08-26T16:11:19.818946Z".toDateTime)(
      fatherMotherLink.createdAt
    )
    assertResult(relatesConcept.id)(fatherMotherLink.relation)

    val fatherChildLink =
      ConceptLink.findBy(sqls"source = ${"1"} and target = ${"3"}").get
    assertResult(true)(fatherChildLink.directionalp)
    assertResult(containsConcept.id)(fatherChildLink.relation)

    val motherChildLink =
      ConceptLink.findBy(sqls"source = ${"2"} and target = ${"3"}").get
    assertResult(true)(motherChildLink.directionalp)
    assertResult(containsConcept.id)(motherChildLink.relation)
  }
}
