defmodule SilverBrain.Config.Repo do
  @type t :: %__MODULE__{
          database: String.t()
        }

  defstruct [:database]
end
