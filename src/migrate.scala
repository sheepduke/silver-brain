// package silver_brain.deprecated

import com.github.ksuid.Ksuid
import com.github.plokhotnyuk.jsoniter_scala.core as jsoniter
import com.github.plokhotnyuk.jsoniter_scala.macros._
import org.flywaydb.core.Flyway
import scalikejdbc.ConnectionPool
import scalikejdbc.DB
import scalikejdbc.DBSession
import scalikejdbc.WrappedResultSet
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef
import silver_brain.core.*
import silver_brain.sql.*

import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import scala.collection.mutable.HashMap
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Try

def migrateOldData() =
  given ExecutionContext = ExecutionContext.global
  given itemCodec: jsoniter.JsonValueCodec[Item] = JsonCodecMaker.make

  val store = SqliteStore(os.home / "temp" / "test")

  migrateDatabase(store, "main")

  val db = store.getSession(using "main").right.get
  db.autoClose(false)

  val idMap = HashMap[String, String]()
  val items = HashMap[String, Item]()

  // Migrate the items.
  for
    oldDb <- store.getSession(using "old")
    concepts <- oldDb.localTx(implicit session => getConcepts())
    concept <- concepts
  do
    val id = Ksuid.fromInstant(concept.createTime).toString()
    idMap.addOne((concept.id, id))

    val item = Item(
      id = id,
      name = concept.name,
      contentType = Some(concept.contentType),
      content = Some(concept.content),
      createTime = Some(concept.createTime),
      updateTime = Some(concept.updateTime)
    )
    items.addOne(id, item)

    val json = jsoniter.writeToString(item)

    db.autoCommit(implicit session =>
      sql"insert into item values($id, $json)".update.apply()
    )

  // Migrate parent links.
  for
    oldDb <- store.getSession(using "old")
    links <- oldDb.localTx(implicit session => getLinks())
    link <- links
  do
    val id = Ksuid.fromInstant(link.createTime).toString()
    val source = idMap(link.source)
    val target = idMap(link.target)

    db.autoCommit(implicit session =>
      link.relation match
        // Contains.
        case "7772fb2f-4b48-4150-9690-6d8dc46773ec" =>
          sql"""
          insert into item_child values(
            $source, $target,
            ${link.createTime.toString()})
        """.update.apply()

        // Uses.
        case "344f5f00-1db8-4639-9024-5ce05093b9cb" =>
          sql"""
        insert into item_reference values($id, $source, $target, 'Uses', ${link.createTime
              .toString()}, ${link.updateTime.toString()})
        """.update.apply()

        // Relates.
        case "2349acb9-be08-4238-81cc-39b302860f2e" =>
          sql"""
         insert into item_reference values(
           $id, $source, $target, 'Relates to',
           ${link.createTime.toString()}, ${link.updateTime.toString()} )
         """.update.apply()
    )

def migrateDatabase(store: SqliteStore, storeName: StoreName) =
  val flyway =
    Flyway
      .configure()
      .locations("classpath:migrations")
      .dataSource(store.jdbcUrl(storeName), null, null)
      .load()

  flyway.migrate()

// ============================================================
//  [Old] Concept
// ============================================================

case class Concept(
    id: String,
    name: String,
    contentType: String,
    content: String,
    createTime: Instant,
    updateTime: Instant
)

def getConcepts()(using session: DBSession): ServiceResponse[List[Concept]] =
  Try(
    sql"select * from concept"
      .map(_.toConcept)
      .list
      .apply()
  ).toServiceResponse

extension (rs: WrappedResultSet)
  def toConcept: Concept =
    Concept(
      id = rs.string("id"),
      name = rs.string("name"),
      contentType = rs.string("content_type"),
      content = rs.string("content"),
      createTime = Instant.parse(rs.string("created_at").replace(' ', 'T')),
      updateTime = Instant.parse(rs.string("updated_at").replace(' ', 'T'))
    )

// ============================================================
//  [Old] Link
// ============================================================

case class Link(
    id: String,
    source: String,
    relation: String,
    target: String,
    isDirectional: Boolean,
    createTime: Instant,
    updateTime: Instant
)

def getLinks()(using session: DBSession): ServiceResponse[List[Link]] =
  Try(
    sql"select * from concept_link"
      .map(_.toLink)
      .list
      .apply()
  ).toServiceResponse

extension (rs: WrappedResultSet)
  def toLink: Link =
    Link(
      id = rs.string("id"),
      source = rs.string("source"),
      relation = rs.string("relation"),
      target = rs.string("target"),
      isDirectional = rs.boolean("directionalp"),
      createTime = Instant.parse(rs.string("created_at").replace(' ', 'T')),
      updateTime = Instant.parse(rs.string("updated_at").replace(' ', 'T'))
    )
