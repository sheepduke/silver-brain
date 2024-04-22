package silver_brain.core

import scala.util.Try
import java.time.Instant
import com.github.ksuid.Ksuid

type Id = String

type StoreName = String

case class Item(
    id: Id,
    name: String,
    contentType: Option[String] = None,
    content: Option[String] = None,
    parents: Option[List[Id]] = None,
    children: Option[List[Id]] = None,
    references: Option[List[Relation]] = None,
    referenced: Option[List[Relation]] = None,
    createTime: Option[Instant] = None,
    updateTime: Option[Instant] = None
)

case class Relation(
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
