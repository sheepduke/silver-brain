package db.common

import scalikejdbc._
import silver_brain.common._

import java.sql.Connection

class DummyStoreConnector(session: DBSession) extends StoreConnector {
  override def withReadOnly[A](fun: DBSession => A)(using DatabaseName): A = {
    fun(session)
  }

  override def withTransaction[A](
      fun: DBSession => A
  )(using DatabaseName): A = {
    fun(session)
  }

  override def withRawConnection[A](fun: Connection => A)(using
      DatabaseName
  ): A = ???
}
