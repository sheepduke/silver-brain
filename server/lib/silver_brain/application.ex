defmodule SilverBrain.Application do
  @moduledoc false

  use Application

  @impl Application
  def start(_type, _args) do
    config = SilverBrain.Config.get()

    children = [
      # Starts a worker by calling: SilverBrain.Worker.start_link(arg)
      {SilverBrain.Repo, [database: config.store.database_file]}
    ]

    opts = [strategy: :one_for_one, name: SilverBrain.Supervisor]
    Supervisor.start_link(children, opts)

    # Run Ecto migrations.
    Ecto.Migrator.run(SilverBrain.Repo, config.app.migration_dir, :up, all: true)
  end

  @impl Application
  def stop(_state) do
    Supervisor.stop(SilverBrain.Supervisor)
  end
end
