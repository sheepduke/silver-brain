defmodule SilverBrain.Repo do
  use Ecto.Repo,
    otp_app: :silver_brain,
    adapter: Ecto.Adapters.SQLite3

  def init(_context, config) do
    if Keyword.has_key?(config, :database) do
      {:ok, config}
    else
      app_config = SilverBrain.Config.get()
      {:ok, Keyword.put(config, :database, app_config.store_database_file)}
    end
  end
end
