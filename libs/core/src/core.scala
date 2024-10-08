package silver_brain.core

import scala.util.Try
import java.time.Instant

case class Item(
    id: String,
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None,
    properties: Option[Map[String, String]] = None,
    parents: Option[Seq[String]] = None,
    children: Option[Seq[String]] = None,
    siblings: Option[Seq[String]] = None,
    createTime: Option[Instant] = None,
    updateTime: Option[Instant] = None
)

enum ItemSelect:
  case Id
  case Name
  case ContentType
  case Content
  case Properties
  case Parents
  case Children
  case Siblings
  case CreateTime
  case UpdateTime

case class Reference(
    id: String,
    source: String,
    target: String,
    annotation: String,
    createTime: Instant,
    updateTime: Instant
)

enum StoreError:
  case StoreNotFound(name: String)
  case IdNotFound(id: String)
  case Conflict(message: String)
  case InvalidArgument(message: String)
  case DataMigrationError(message: String)
  case InternalError(message: String)

type StoreResult[A] = Either[StoreError, A]

extension [A](result: Try[A])
  def toStoreResult: StoreResult[A] =
    result.toEither.left.map(error =>
      StoreError.InternalError(
        error.getMessage() + "\n" + error.getStackTrace().mkString("\n")
      )
    )
