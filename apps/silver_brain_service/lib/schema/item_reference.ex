defmodule SilverBrain.Service.Schema.ItemReference do
  use Ecto.Schema

  import Ecto.Changeset

  @primary_key {:id, :string, autogenerate: false}
  schema "item_references" do
    field :source, :string
    field :target, :string
    field :annotation, :string
    field :create_time, :utc_datetime
    field :update_time, :utc_datetime
  end
end
