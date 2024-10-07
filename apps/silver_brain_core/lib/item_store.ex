defprotocol SilverBrain.Core.ItemStore do
  import SilverBrain.Core

  alias SilverBrain.Core.Item

  @type result(t) :: SilverBrain.Core.result(t)
  @type result :: SilverBrain.Core.result()

  # ============================================================
  #  Item
  # ============================================================

  defmodule UpdateItem do
    use TypedStruct

    typedstruct do
      field :id, String.t()
      field :name, String.t()
    end
  end

  @spec get_item(t, Item.id()) :: result(Item.t())
  def get_item(store, item_id)

  @spec get_item(t, Item.id(), [atom()]) :: result(Item.t())
  def get_item(store, item_id, select)

  @spec get_items(t, list(Item.id()), select: atom()) :: result([Item.t()])
  def get_items(store, item_ids, select)

  @spec create_item(t, String.t()) :: result(Item.id())
  def create_item(store, name)

  @spec update_item(t, UpdateItem.t()) :: result()
  def update_item(store, item)

  @spec delete_item(t, Item.id()) :: result()
  def delete_item(store, item_id)

  # ============================================================
  #  Property
  # ============================================================

  @spec create_item_property(t, Item.id(), String.t(), String.t()) :: result()
  def create_item_property(store, item_id, key, value)

  @spec delete_item_property(t, Item.id(), String.t()) :: result()
  def delete_item_property(store, item_id, key)

  # ============================================================
  #  Link
  # ============================================================

  @spec create_child(t, Item.id(), Item.id()) :: result()
  def create_child(store, parent_id, child_id)

  @spec get_parents(t, Item.id()) :: result([Item.id()])
  def get_parents(store, item_id)

  @spec get_children(t, Item.id()) :: result([Item.id()])
  def get_children(store, item_id)

  @spec delete_child(t, Item.id(), Item.id()) :: result()
  def delete_child(store, parent_id, child_id)
end
