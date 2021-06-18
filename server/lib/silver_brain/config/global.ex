defmodule SilverBrain.Config.Global do
  @type t :: %__MODULE__{
          config_file: [String.t()]
        }

  defstruct [:config_file]
end
