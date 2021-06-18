defmodule SilverBrain.Config do
  @env_key :silver_brain

  @doc """
  Get configuration struct from application environment.
  """
  @spec get(module) :: struct
  def get(struct) do
    Application.get_env(@env_key, struct)
  end

  @doc """
  Put configuration struct to the application environment.
  """
  @spec put(struct) :: :ok
  def put(struct = %key{}) do
    Application.put_env(@env_key, key, struct)
  end
end
