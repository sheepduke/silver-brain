defmodule SilverBrain.Application do
  @moduledoc false

  use Application

  @impl Application
  def start(_type, _args) do
    config = SilverBrain.Config.get()

    children = [
      {Plug.Cowboy, scheme: :http, plug: SilverBrain.Web, options: [port: 4001]},

      # Starts a worker by calling: SilverBrain.Worker.start_link(arg)
      {SilverBrain.Repo, [database: config.store_database_file]}
    ]

    opts = [strategy: :one_for_one, name: SilverBrain.Supervisor]
    return = Supervisor.start_link(children, opts)

    # Run Ecto migrations.
    Ecto.Migrator.run(SilverBrain.Repo, config.app_migration_dir, :up, all: true)

    return
  end

  @impl Application
  def stop(_state) do
    Supervisor.stop(SilverBrain.Supervisor)
  end

  @doc """
  Restart the application.
  """
  def restart() do
    stop(:killed)
    start(:normal, [])
  end
end
