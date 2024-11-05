package silverbrain.core

import scala.util.Try

type AppResult[A] = Either[AppError, A]

extension [A](result: Try[A])
  def toAppResult: AppResult[A] =
    result.toEither.left.map(AppInternalError.apply)
