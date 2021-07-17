defmodule SilverBrain.Repo do
  use Ecto.Repo,
    otp_app: :silver_brain,
    adapter: Ecto.Adapters.SQLite3

  def init(_type, config) do
    # Merge custom configuration before starting database.
    app_config = SilverBrain.Config.get()

    repo_config = [
      database: app_config.store.database_file,
      migration_primary_key: false,
      migration_timestamps: [type: :utc_datetime_usec]
    ]

    {:ok, Keyword.merge(config, repo_config)}
  end
end
