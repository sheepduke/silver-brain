# This file is responsible for configuring your umbrella
# and **all applications** and their dependencies with the
# help of the Config module.
#
# Note that all applications in your umbrella share the
# same configuration and dependencies, which is why they
# all use the same configuration file. If you want different
# configurations or dependencies per app, it is best to
# move said applications out of the umbrella.
import Config

config :silver_brain_service,
  data_root_path: "~/temp/test",
  ecto_repos: [SilverBrain.Service.Repo]

config :silver_brain_service, SilverBrain.Service.Repo,
  name: nil,
  migration_primary_key: false,
  migration_timestamps: [
    type: :utc_datetime,
    inserted_at: :create_time,
    updated_at: :update_time
  ]

# Sample configuration:
#
#     config :logger, :console,
#       level: :info,
#       format: "$date $time [$level] $metadata$message\n",
#       metadata: [:user_id]
#
