defprotocol SilverBrain.Core.ItemStore do
  import SilverBrain.Core

  alias SilverBrain.Core.Item

  @type result(t) :: SilverBrain.Core.result(t)
  @type result :: SilverBrain.Core.result()

  # ============================================================
  #  Item
  # ============================================================

  @spec get_item(t, Item.id()) :: result(Item.t())
  def get_item(store, item_id)

  @spec get_item(t, Item.id(), [atom()]) :: result(Item.t())
  def get_item(store, item_id, select)

  @spec get_items(t, list(Item.id()), select: atom()) :: result([Item.t()])
  def get_items(store, item_ids, select)

  @spec create_item(t, String.t()) :: result(Item.id())
  def create_item(store, name)

  @spec update_item(t, Item.t()) :: result()
  def update_item(store, item)

  @spec delete_item(t, Item.id()) :: result()
  def delete_item(store, item_id)

  # ============================================================
  #  Link
  # ============================================================

  @spec create_child(t, Item.id(), Item.id()) :: result()
  def create_child(store, item_id, child_id)

  @spec get_parents(t, Item.id()) :: result([Item.id()])
  def get_parents(store, item_id)

  @spec get_children(t, Item.id()) :: result([Item.id()])
  def get_children(store, item_id)

  @spec delete_child(t, Item.id(), Item.id()) :: result()
  def delete_child(store, item_id, child_id)
end
