package silver_brain.common

import scalikejdbc._

import java.sql.Connection

type DatabaseName = String

trait StoreConnector {
  def withReadOnly[A](fun: DBSession => A)(using DatabaseName): A

  def withTransaction[A](fun: DBSession => A)(using DatabaseName): A

  def withRawConnection[A](fun: Connection => A)(using DatabaseName): A
}
