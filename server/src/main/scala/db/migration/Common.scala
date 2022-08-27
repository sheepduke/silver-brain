package db.migration

import org.flywaydb.core.api.migration.BaseJavaMigration
import org.flywaydb.core.api.migration.Context
import scalikejdbc._

trait DbSessionMigration extends BaseJavaMigration {
  def migrate()(using DBSession): Unit

  override def migrate(context: Context): Unit = {
    context.getDb().withinTx { implicit session =>
      migrate()
    }
  }

  extension (context: Context) {
    def getDb(): DB = {
      val db = DB(context.getConnection())
      db.autoClose(false)

      db
    }
  }
}
