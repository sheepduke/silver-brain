defmodule SilverBrain.Service.Repo do
  use Ecto.Repo,
    otp_app: :silver_brain_service,
    adapter: Ecto.Adapters.SQLite3
end
