defmodule SilverBrain.Application do
  alias Vapor.Provider.File

  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    config = load_config()

    children = [
      # Starts a worker by calling: SilverBrain.Worker.start_link(arg)
      # {SilverBrain.Worker, arg}
      {SilverBrain.Repo, [database: config.database]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: SilverBrain.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def load_config() do
    providers = [
      %File{
        path: "config.toml",
        bindings: [
          title: "title",
          database: ["Database", "database"]
        ]
      }
    ]

    config = Vapor.load!(providers)

    IO.inspect(config)
  end
end

config = SilverBrain.Application.load_config()

# SilverBrain.Application.start(:normal, [])
# SilverBrain.Application.stop(:killed)
