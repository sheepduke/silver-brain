defmodule SilverBrain.Core.Item do
  use TypedStruct

  @type id() :: String.t()

  typedstruct do
    field :id, id()
    field :name, String.t()
    field :content_type, String.t()
    field :content, String.t()
    field :create_time, String.t()
    field :update_time, String.t()
    field :properties, %{String.t() => String.t()}
    field :parents, [id()]
    field :children, [id()]
  end
end
