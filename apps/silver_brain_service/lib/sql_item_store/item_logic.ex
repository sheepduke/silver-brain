defmodule SilverBrain.Service.SqlItemStore.ItemLogic do
  alias SilverBrain.Core.Item
  alias SilverBrain.Service.Repo
  alias SilverBrain.Service.RepoManager
  alias SilverBrain.Service.Schema
  alias SilverBrain.Service.SqlItemStore

  import Ecto.Query, only: [from: 2]

  def get_item(store, item_id) do
    internal_get_item(store, item_id)
  end

  def get_item(store, item_id, select) do
    internal_get_item(store, item_id, select)
  end

  defp internal_get_item(store, item_id, select \\ nil) do
    query_fun =
      if select == nil do
        fn -> Repo.one(from(Schema.Item, where: [id: ^item_id])) end
      else
        fn -> Repo.one(from(Schema.Item, where: [id: ^item_id], select: ^select)) end
      end

    with :ok <- RepoManager.connect(store.repo_name) do
      case query_fun.() do
        db_item ->
          %Item{
            id: db_item.id,
            name: db_item.name,
            content_type: db_item.content_type,
            content: db_item.content,
            create_time: db_item.create_time,
            update_time: db_item.update_time
          }

        nil ->
          {:error, :not_found}
      end
    end
  end
end
