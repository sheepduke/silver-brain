import Config

config :silver_brain,
  ecto_repos: [SilverBrain.Repo]

config :silver_brain, SilverBrain.Repo,
  migration_primary_key: false,
  migration_timestamps: [type: :utc_datetime_usec]

config :silver_brain, :environment, Mix.env()
