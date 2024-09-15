defmodule SilverBrain.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      version: "0.1.0",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def common_deps,
    do: [
      # For better struct definition.
      {:typed_struct, "~> 0.1.4"},

      # Dev only.
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false}
    ]

  # Dependencies listed here are available only for this
  # project and cannot be accessed from applications inside
  # the apps folder.
  #
  # Run "mix help deps" for examples and options.
  defp deps do
    []
  end
end
