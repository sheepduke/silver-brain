defmodule SilverBrain.Service.Schema.ItemProperty do
  use Ecto.Schema

  schema "item_properties" do
    field :name, :string
    field :content_type, :string
    field :content, :string
    field :create_time, :utc_datetime
    field :update_time, :utc_datetime

    belongs_to(:item, SilverBrain.Service.Schema.Item)
  end
end


