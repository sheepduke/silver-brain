defmodule SilverBrain.Repo do
  use Ecto.Repo,
    otp_app: :silver_brain,
    adapter: Ecto.Adapters.SQLite3
end
