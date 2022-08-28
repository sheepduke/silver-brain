package db.migration

import db.common._
import db.model.v1_5._
import org.scalatest.flatspec.FixtureAnyFlatSpec
import scalikejdbc._
import scalikejdbc.scalatest.AutoRollback

class MigrationV1_6Spec extends FixtureAnyFlatSpec with AutoRollback {
  override def db() = InMemorySqliteDb.create()

  override def fixture(using DBSession) = {}

  behavior of "Empty database"

  it should "migrate successfully" in { implicit session =>
    MigrationRunner.run("1.6")

    intercept[org.sqlite.SQLiteException] {
      Concept.countAll()
    }
    assertResult(0)(
      sql"select count(1) from concept".map(_.int(1)).single.apply().get
    )
    assertResult(0)(ConceptLink.countAll())
  }
}
