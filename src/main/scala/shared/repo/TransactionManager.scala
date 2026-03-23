package silverbrain.shared.repo

trait Transaction

trait TransactionManager:
  def inTransaction[A](
      action: Transaction ?=> Either[Error, A]
  ): Either[Error, A]
