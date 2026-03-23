package silverbrain

import silverbrain.item.domain.*
import silverbrain.item.repo.*
import silverbrain.shared.repo.*

import java.nio.file.{Files, Path}
import java.sql.DriverManager
import java.time.Clock
import org.flywaydb.core.Flyway
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import scala.jdk.CollectionConverters.*
import scalasql.DbClient
import scalasql.DbClient.DataSource
import scalasql.SqliteDialect.*

@main def hello(): Unit =
  given Clock = Clock.systemUTC()

  // val flyway = Flyway
  //   .configure()
  //   .dataSource("jdbc:sqlite:./silver-brain.db", "", "")
  //   .load()

  // flyway.migrate()

  val dataSource = SQLiteDataSource()
  dataSource.setUrl("jdbc:sqlite:./silver-brain.db")
  val dbClient = DataSource(dataSource)

  val tm = SqlTransactionManager(dbClient)
  val repo = SqlItemRepo()
