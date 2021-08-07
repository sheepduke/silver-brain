defmodule SilverBrain.ConceptMap.Concept do
  @type t :: %__MODULE__{
          uuid: String.t(),
          name: String.t(),
          content_type: nil | String.t(),
          content: nil | String.t(),
          create_time: DateTime.t(),
          update_time: DateTime.t()
        }

  @derive Jason.Encoder
  defstruct [:uuid, :name, :content_type, :content, :create_time, :update_time]
end
