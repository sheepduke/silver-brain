package silver_brain.sqlite_store.dao

import java.time.Instant
import scalikejdbc.*
import com.github.ksuid.Ksuid

case class Item(
    id: String,
    name: String,
    contentType: String,
    content: String,
    createTime: Instant,
    updateTime: Instant
)

object Item extends SQLSyntaxSupport[Item]:
  def newId = "i_" + Ksuid.newKsuid()

  def apply(item: ResultName[Item])(rs: WrappedResultSet): Item =
    Item(
      rs.string(item.id),
      rs.string(item.name),
      rs.string(item.contentType),
      rs.string(item.content),
      Instant.parse(rs.string(item.createTime)),
      Instant.parse(rs.string(item.updateTime))
    )

  override def columns: Seq[String] =
    Seq("id", "name", "content_type", "content", "create_time", "update_time")
