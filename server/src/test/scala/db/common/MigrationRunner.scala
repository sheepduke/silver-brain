package db.common

import scalikejdbc._

import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.util.Using
import db.migration._

object MigrationRunner {
  private val migrationTable: Map[String, DBSession => Unit] = SortedMap(
    "1" -> (implicit s => executeSqlMigration("V1__Initial_version.sql")),
    "1.5" -> (implicit s => V1_5__Upgrade_schema().migrate()),
    "1.6" -> (implicit s =>
      executeSqlMigration("V1_6__Cleanup_old_tables.sql")
    ),
    "2" -> (implicit s => executeSqlMigration("V2__Adjust_columns.sql"))
  )

  def run()(using DBSession): Unit = {
    run(migrationTable.keys.last)
  }

  def run(maxVersion: String)(using DBSession): Unit = {
    val minVersion = migrationTable.keys.take(1).head

    run(minVersion, maxVersion)
  }

  def run(minVersion: String, maxVersion: String)(using
      session: DBSession
  ): Unit = {
    for (version, fun) <- migrationTable
    do if version >= minVersion && version <= maxVersion then fun(session)
  }

  def runSingle(version: String)(using session: DBSession): Unit = {
    migrationTable.get(version).get.apply(session)
  }

  private def executeSqlMigration(
      fileName: String
  )(using session: DBSession): Unit = {
    val sql = Source
      .fromResource(s"db/migration/$fileName")
      .getLines()
      .toSeq
      .mkString("\n")

    Using.resource(session.connection.createStatement()) { statement =>
      statement.executeUpdate(sql)
    }
  }
}
