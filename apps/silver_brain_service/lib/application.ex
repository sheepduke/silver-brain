defmodule SilverBrain.Service.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # {SilverBrain.Service.RepoManager,
      #  Path.expand(Application.get_env(:silver_brain_service, :data_root_path))}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: SilverBrain.Service.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
