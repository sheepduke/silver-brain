defmodule SilverBrain.Service.Schema.ItemLink do
  use Ecto.Schema

  import Ecto.Changeset

  @primary_key false
  schema "item_links" do
    field :parent, :string, primary_key: true
    field :child, :string, primary_key: true
    field :create_time, :utc_datetime
    field :update_time, :utc_datetime
  end
end
