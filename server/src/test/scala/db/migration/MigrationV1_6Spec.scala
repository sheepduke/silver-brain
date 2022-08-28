package db.migration

import org.scalatest.flatspec.FixtureAnyFlatSpec
import scalikejdbc.scalatest.AutoRollback
import db.model.v1_5.Concept
import db.model.v1_5.ConceptLink
import scalikejdbc._

class MigrationV1_6Spec extends FixtureAnyFlatSpec with AutoRollback {
  override def db() = inMemorySqliteDb()

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
