defmodule SilverBrain.Service.Schema.Item do
  use Ecto.Schema

  @primary_key false
  schema "item" do
    field :id, :string, primary_key: true
    field :props, :string
  end
end
