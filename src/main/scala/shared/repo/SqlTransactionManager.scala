package silverbrain.shared.repo

import scalasql.{DbApi, DbClient}
import silverbrain.shared.repo.{Transaction, TransactionManager}

class SqlTransaction(val dbApi: DbApi) extends Transaction

extension (tx: Transaction)
  def dbApi: DbApi = tx.asInstanceOf[SqlTransaction].dbApi

final class SqlTransactionManager(db: DbClient) extends TransactionManager:
  private class RollbackMarker(val left: Any) extends Exception

  override def inTransaction[A](
      action: Transaction ?=> Either[Error, A]
  ): Either[Error, A] =
    try
      db.transaction: dbApi =>
        given SqlTransaction(dbApi)

        action match
          case right @ Right(_) => right
          case left @ Left(_)   => throw RollbackMarker(left)

    catch case e: RollbackMarker => e.left.asInstanceOf[Either[Error, A]]
