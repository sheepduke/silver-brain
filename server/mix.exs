defmodule SilverBrain.MixProject do
  use Mix.Project

  def project do
    [
      app: :silver_brain,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {SilverBrain.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # Database.
      {:ecto_sql, "~> 3.0"},
      {:ecto_sqlite3, "~> 0.5.5"},

      # JSON serializer.
      {:jason, "~> 1.2"},

      # Web server.
      {:plug_cowboy, "~> 2.0"},

      # Configuration adapter.
      {:vapor, "~> 0.10"},

      # ----------------------------------------------------------------------

      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},

      # Http client.
      {:tesla, "~> 1.4", only: [:dev, :test]},
      {:hackney, "~> 1.17", only: [:dev, :test]}
    ]
  end
end
