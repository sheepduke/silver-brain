defmodule SilverBrain.Repo do
  use Ecto.Repo,
    otp_app: :silver_brain,
    adapter: Ecto.Adapters.SQLite3

  def init(_context, config) do
  end
end

# Ecto.Adapters.SQL.query!(SilverBrain.Repo, "select count(*) from concept")

# SilverBrain.Repo.
