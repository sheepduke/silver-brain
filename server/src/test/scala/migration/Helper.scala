package silver_brain
package migration
import com.github.nscala_time.time.Imports._
import common._
import db.migration.V2_0__Upgrade_schema
import scalikejdbc._

import scala.io.Source
import scala.util.Using

object Helper {
  def withTempDatabase(
      fun: (StoreConnector, DatabaseName) => Unit
  ) = {
    val databaseDir = os.temp.dir()
    val databaseName = databaseDir.baseName.toString

    val storeConnector = SqliteStoreConnector()(using
      DatabaseConfig(
        rootDir = databaseDir,
        defaultDatabaseName = databaseName
      )
    )

    fun(storeConnector, databaseName)
  }

  def migrateV1()(using
      storeConnector: StoreConnector
  )(using DatabaseName) = {
    executeSqlMigration(storeConnector, "V1__Initial_version.sql")
  }

  def migrateV2()(using
      storeConnector: StoreConnector
  )(using DatabaseName) = {
    storeConnector.withTransaction { implicit session =>
      V2_0__Upgrade_schema().migrate()
    }
  }

  def getConcepts()(using
      storeConnector: StoreConnector
  )(using DatabaseName): Int = {
    storeConnector.withReadOnly { implicit session =>
      sql"select count(*) from concept"
        .map(rs => rs.int(1))
        .single
        .apply()
        .get
    }
  }

  private def executeSqlMigration(
      storeConnector: StoreConnector,
      fileName: String
  )(using DatabaseName): Unit = {
    val sql = Source
      .fromResource(s"db/migration/$fileName")
      .getLines()
      .toSeq
      .mkString("\n")

    println("SQL executed:")
    println(sql)

    storeConnector.withRawConnection { connection =>
      Using.resource(connection.createStatement()) { statement =>
        statement.executeUpdate(sql)
      }
    }
  }
}
