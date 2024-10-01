defmodule SilverBrain.Core do
  @type result :: :ok | {:error, atom(), String.t()} 

  @type result(t) :: {:ok, t} | {:error, atom(), String.t()}
end
