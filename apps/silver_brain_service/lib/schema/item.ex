defmodule SilverBrain.Service.Schema.Item do
  use Ecto.Schema

  import Ecto.Changeset

  @primary_key false
  schema "items" do
    field :id, :string, primary_key: true
    field :name, :string
    field :content_type, :string
    field :content, :string
    field :create_time, :utc_datetime
    field :update_time, :utc_datetime
  end
end
