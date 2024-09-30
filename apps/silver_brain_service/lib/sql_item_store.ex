defmodule SilverBrain.Service.SqlItemStore do
  use TypedStruct
  alias SilverBrain.Core.Item
  alias SilverBrain.Service.RepoManager

  import Ecto.Query, only: [from: 2]

  typedstruct do
    field :repo_name, String.t(), enforce: true
  end
end

# store = %SilverBrain.Service.SqlItemStore{repo_name: "main"}
# SilverBrain.Core.ItemStore.get_item(store, "i_1KECzKOCyJlvx0kGfvyPJAU030T")

defimpl SilverBrain.Core.ItemStore, for: SilverBrain.Service.SqlItemStore do
  alias SilverBrain.Core.ItemStore
  alias SilverBrain.Core.Item
  alias SilverBrain.Service.SqlItemStore
  alias SilverBrain.Service.Repo
  alias SilverBrain.Service.RepoManager
  alias SilverBrain.Service.Schema

  import Ecto.Query, only: [from: 2]

  require Item

  # ============================================================
  #  Item
  # ============================================================

  def create_item(store, name) do
    with _ <- RepoManager.connect(store.repo_name) do
      SqlItemStore.ItemLogic.create_item(store, name)
    end
  end

  def get_item(store, item_id) do
    with _ <- RepoManager.connect(store.repo_name) do
      SqlItemStore.ItemLogic.get_item(store, item_id)
    end
  end

  def get_item(store, item_id, select) do
    with _ <- RepoManager.connect(store.repo_name) do
      SqlItemStore.ItemLogic.get_item(store, item_id, select)
    end
  end

  def get_items(store, item_ids, select) do
  end

  def update_item(%SqlItemStore{repo_name: repo_name}, item) do
    with _ <- RepoManager.connect(repo_name) do
      SqlItemStore.ItemLogic.update_item(item)
    end
  end

  def delete_item(store, item_id) do
  end

  # ============================================================
  #  Property
  # ============================================================

  def create_item_property(store, item_id, key, value) do
    property = %Schema.ItemProperty{
      item_id: item_id,
      key: key,
      value: value
    }

    Repo.insert(Schema.ItemProperty, property)
  end

  def delete_item_property(store, item_id, key) do
    from(property in Schema.ItemProperty,
      where: property.item_id == ^item_id and property.key == ^key
    )
    |> Repo.delete_all()
  end

  # ============================================================
  #  Link
  # ============================================================

  def create_child(store, item_id, child_id) do
  end

  def get_parents(store, item_id) do
  end

  def get_children(store, item_id) do
  end

  def delete_child(store, item_id, child_id) do
  end
end
