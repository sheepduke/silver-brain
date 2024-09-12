defprotocol SilverBrain.Core.ItemStore do
  import SilverBrain.Core
  alias SilverBrain.Core.Item

  @type result(t) :: SilverBrain.Core.result(t)
  @type result :: SilverBrain.Core.result()

  @spec get_item(t, Item.id(), select: atom()) :: result(Item.t())
  def get_item(store, item_id, select)

  @spec get_items(t, list(Item.id()), select: atom()) :: result([Item.t()])
  def get_items(store, item_ids, select)

  @spec create_item(t, String.t()) :: result(Item.t())
  def create_item(store, name)

  @spec update_item(t, Item.t()) :: result(Item.t())
  def update_item(store, item)

  @spec delete_item(t, Item.id()) :: result()
  def delete_item(store, item_id)
end
