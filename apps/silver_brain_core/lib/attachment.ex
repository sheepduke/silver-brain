defmodule SilverBrain.Core.Attachment do
  use TypedStruct

  @type id() :: String.t()

  typedstruct do
    field :id, id()
    field :name, String.t()
    field :file_path, Path.t()
  end
end
