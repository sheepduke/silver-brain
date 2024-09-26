defmodule SilverBrain.Service.SqlItemStoreTest do
  use ExUnit.Case

  alias SilverBrain.Core.ItemStore
  alias SilverBrain.Service.RepoManager

  setup context do
    Application.stop(:silver_brain_service)

    {:ok, _} = start_supervised({RepoManager, context[:tmp_dir]})
    RepoManager.create("main")
    item_store = %SilverBrain.Service.SqlItemStore{repo_name: "main"}

    [item_store: item_store]
  end

  @tag :tmp_dir
  test "create item", context do
    item_store = context[:item_store]

    {:ok, item_id} = ItemStore.create_item(item_store, "Emacs")

    item = ItemStore.get_item(item_store, item_id)

    assert item.id == item_id
    assert item.name == "Emacs"
    assert item.create_time == item.update_time
  end
end

# ExUnit.run()

# Application.stop(:silver_brain_service)
# ExUnit.start()
