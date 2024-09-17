defmodule SilverBrain.Service.Schema.ItemReference do
  use Ecto.Schema

  import Ecto.Changeset

  @primary_key false
  schema "item_references" do
    field :id, :string, primary_key: true
    field :source, :string
    field :target, :string
    field :annotation, :string
    field :create_time, :utc_datetime
    field :update_time, :utc_datetime
  end
end
