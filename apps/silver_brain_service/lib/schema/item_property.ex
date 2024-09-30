defmodule SilverBrain.Service.Schema.ItemProperty do
  use Ecto.Schema

  @primary_key false
  schema "item_properties" do
    field :item_id, :string, primary_key: true
    field :key, :string, primary_key: true
    field :value, :string
  end
end
