defmodule SilverBrain.Service.Schema.Item do
  use Ecto.Schema

  @primary_key {:id, :string, autogenerate: false}
  schema "items" do
    field :name, :string
    field :content_type, :string
    field :content, :string
    field :create_time, :utc_datetime
    field :update_time, :utc_datetime

    has_many(:properties, SilverBrain.Service.Schema.ItemProperty)
  end
end
