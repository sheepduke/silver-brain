package silverbrain.item.application

import silverbrain.item.repo.ItemRepo
import silverbrain.shared.repo.TransactionManager

class RenameItem(transactionManager: TransactionManager, repo: ItemRepo):
  def execute(): Either[Error, Unit] =
    ???
