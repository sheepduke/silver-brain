defmodule SilverBrain.Core.ItemReference do
  use TypedStruct

  @type id() :: String.t()

  typedstruct do
    field :id, id()
    field :source, id()
    field :target, id()
    field :annotation, String.t()
  end
end
