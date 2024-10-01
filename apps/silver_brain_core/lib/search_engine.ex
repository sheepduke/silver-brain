defprotocol SilverBrain.Core.SearchEngine do
  alias SilverBrain.Core.Item

  @spec search(t, String.t()) :: list(Item.id())
  def search(store, search_string)
end
