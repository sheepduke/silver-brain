package silver_brain.core

import scala.util.Try
import java.time.Instant

type Id = String

type StoreName = String

case class Item(
    id: Id,
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None,
    properties: Option[Map[String, String]] = None,
    parents: Option[Seq[Id]] = None,
    children: Option[Seq[Id]] = None,
    siblings: Option[Seq[Id]] = None,
    referencesFromThis: Option[Seq[Id]] = None,
    referencesToThis: Option[Seq[Id]] = None,
    createTime: Option[Instant] = None,
    updateTime: Option[Instant] = None
)

case class ItemLoadOptions(
    loadContentType: Boolean = false,
    loadContent: Boolean = false,
    loadCreateTime: Boolean = false,
    loadUpdateTime: Boolean = false,
    loadProperties: Boolean = false,
    loadParents: Boolean = false,
    loadChildren: Boolean = false,
    loadSiblings: Boolean = false,
    loadReferencesFromThis: Boolean = false,
    loadReferencesToThis: Boolean = false
)

case class Reference(
    id: Id,
    source: Id,
    target: Id,
    annotation: String
)

enum ServiceError:
  case StoreNotFound(id: StoreName)
  case IdNotFound(id: Id)
  case Conflict(message: String)
  case InvalidArgument(message: String)
  case InternalError(message: String)

extension [A](result: Try[A])
  def toServiceResponse: ServiceResponse[A] =
    result.toEither.left.map(error =>
      ServiceError.InternalError(
        error.getMessage() + "\n" + error.getStackTrace().mkString("\n")
      )
    )

type ServiceResponse[A] = Either[ServiceError, A]
