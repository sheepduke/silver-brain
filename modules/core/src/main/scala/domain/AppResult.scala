package silverbrain.core

import scala.util.Try
import cats.effect.IO
import scala.util.Success
import scala.util.Failure

type AppIOResult[A] = IO[AppResult[A]]

object AppIOResult:
  def blockingLiftTry[A](thunk: => A): AppIOResult[A] =
    IO.blocking(AppResult.liftTry(thunk))

  def blockingFlatTry[A](thunk: => AppResult[A]): AppIOResult[A] =
    IO.blocking(AppResult.flatTry(thunk))

  def pureLeft[A](error: AppError): IO[Either[AppError, A]] =
    IO.pure(Left(error))

type AppResult[A] = Either[AppError, A]

object AppResult:
  def liftTry[A](thunk: => A): AppResult[A] =
    Try(thunk).toEither.left.map(AppInternalError.apply)

  def flatTry[A](thunk: => AppResult[A]): AppResult[A] =
    Try(thunk) match
      case Success(value)     => value
      case Failure(exception) => Left(AppInternalError(exception))
