defprotocol SilverBrain.Core.SearchEngine do
  @spec search(String.t()) :: tuple()
  def search(search_string)
end
