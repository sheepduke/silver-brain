defmodule SilverBrain.Service.Schema.ItemLink do
  use Ecto.Schema

  import Ecto.Changeset

  @priary_key false
  schema "item_links" do
    field :parent, :string
    field :child, :string
    field :create_time, :utc_datetime
    field :update_time, :utc_datetime
  end
end
