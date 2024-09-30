defprotocol SilverBrain.Core.ItemReferenceStore do
  alias SilverBrain.Core.Item
  alias SilverBrain.Core.ItemReference

  @type result(t) :: SilverBrain.Core.result(t)
  @type result :: SilverBrain.Core.result()

  @spec create_reference(t, ItemReference.t()) :: result(ItemReference.id())
  def create_reference(store, reference_id)

  @spec get_references_from_item(t, Item.t()) :: result(ItemReference.t())
  def get_references_from_item(store, item_id)

  @spec get_references_to_item(t, Item.t()) :: result(ItemReference.t())
  def get_references_to_item(store, item_id)

  @spec update_reference(t, ItemReference.t()) :: result()
  def update_reference(store, reference)

  @spec delete_reference(t, ItemReference.id()) :: result()
  def delete_reference(store, reference_id)
end
