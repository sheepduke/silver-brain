defmodule SilverBrain.Core.ItemLink do
  use TypedStruct

  @type id() :: String.t()

  typedstruct do
    field :parent, id()
    field :child, id()
  end
end
