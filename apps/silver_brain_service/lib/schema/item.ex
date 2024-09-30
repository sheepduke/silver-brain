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

  def create(id, name, content_type, content, create_time, update_time) do
    %__MODULE__{
      id: id,
      name: name,
      content_type: content_type,
      content: content,
      create_time: create_time,
      update_time: update_time
    }
  end
end
