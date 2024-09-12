defmodule SilverBrain.Core do
  @type result :: :ok | {:error, String.t()}

  @type result(t) :: {:ok, t} | {:error, String.t()}
end
