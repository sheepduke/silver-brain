package silver_brain.core

import scala.util.Try
import java.time.Instant
import com.github.ksuid.Ksuid

type Id = String

type StoreName = String

case class Reference(
    id: Id,
    target: Id,
    annotation: String
)

case class Item(
    id: Option[Id] = None,
    name: Option[String] = None,
    contentType: Option[String] = None,
    content: Option[String] = None,
    parents: Option[List[Id]] = None,
    children: Option[List[Id]] = None,
    references: Option[List[Reference]] = None,
    referenced: Option[List[Reference]] = None,
    createTime: Option[Instant] = None,
    updateTime: Option[Instant] = None
)

enum ServiceError:
  case StoreNotFound(id: StoreName)
  case IdNotFound(id: Id)
  case DataAccessError(message: String)
  case DataCorruption(message: String)
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
