defmodule SilverBrain.Server.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: SilverBrainServer.Worker.start_link(arg)
      # {SilverBrainServer.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: SilverBrainServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defimpl Jason.Encoder, for: SilverBrain.Core.Item do
  def encode(value, opts) do
    map =
      Map.from_struct(value)
      |> Map.filter(fn {_key, value} -> value != nil end)

    Jason.Encode.map(map, opts)
  end
end
