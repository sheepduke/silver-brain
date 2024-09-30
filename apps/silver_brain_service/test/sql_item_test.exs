defmodule SilverBrain.Service.SqlItemStoreTest do
  use ExUnit.Case

  alias SilverBrain.Core.Item
  alias SilverBrain.Core.ItemStore
  alias SilverBrain.Service.RepoManager

  setup context do
    Application.stop(:silver_brain_service)

    {:ok, _} = start_supervised({RepoManager, context[:tmp_dir]})
    RepoManager.create("main")
    store = %SilverBrain.Service.SqlItemStore{repo_name: "main"}

    [store: store]
  end

  @tag :tmp_dir
  test "create item and get", context do
    store = context[:store]

    {:ok, item_id} = ItemStore.create_item(store, "Emacs")

    item = ItemStore.get_item(store, item_id)
    assert item.id == item_id
    assert item.name == "Emacs"
    assert item.create_time == item.update_time

    item =
      ItemStore.get_item(store, item_id, [
        :id,
        :name,
        :content_type,
        :content,
        :create_time,
        :update_time,
        :properties
      ])

    assert item.content_type == ""
    assert item.content == ""
    assert item.create_time != nil
    assert item.create_time == item.update_time

    item = ItemStore.get_item(store, item_id, [:name])
    assert item.id == nil
    assert item.content == nil
    assert item.content_type == nil
  end

  @tag :tmp_dir
  test "update item", context do
    store = context[:store]

    {:ok, item_id} = ItemStore.create_item(store, "Vim")

    new_item = %ItemStore.UpdateItem{
      id: item_id,
      name: "Emacs"
    }

    result = ItemStore.update_item(store, new_item)
    assert result == :ok

    item = ItemStore.get_item(store, item_id)
    assert item.name == "Emacs"
  end
end

# Application.stop(:silver_brain_service)
# ExUnit.start()
# ExUnit.run()
