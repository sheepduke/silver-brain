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

  def get_item(store, item_id) do
    SqlItemStore.ItemLogic.get_item(store, item_id)
  end

  def get_item(store, item_id, select) do
    SqlItemStore.ItemLogic.get_item(store, item_id, select)
  end

  def get_items(store, item_ids, select) do
  end

  def create_item(store, name) do
    id = "i_" <> Ksuid.generate()
  end

  def update_item(store, item) do
  end

  def delete_item(store, item_id) do
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
