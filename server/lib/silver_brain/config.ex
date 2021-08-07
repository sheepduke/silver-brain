defmodule SilverBrain.Config do
  @app :silver_brain
  @migration_dir "priv/repo/migrations"
  @env_key :config
  @env_var_config_file "APP_CON FIG_FILE"

  @type t :: %__MODULE__{
          app_migration_dir: String.t(),
          server_port: integer(),
          store_database_file: String.t()
        }

  defstruct [
    :app_migration_dir,
    :server_port,
    :store_database_file
  ]

  @doc """
  Get configuration struct.
  """
  @spec get() :: __MODULE__.t()
  def get() do
    case Application.get_env(@app, @env_key, nil) do
      nil -> init!()
      config -> config
    end
  end

  @doc """
  Load configuration and set it to application environment.
  """
  @spec init!() :: __MODULE__.t()
  def init!() do
    config = %__MODULE__{
      app_migration_dir: Application.app_dir(@app, @migration_dir)
    }

    config_file = get_config_file()
    ensure_config_file!(config_file)

    providers = [
      %Vapor.Provider.File{
        path: config_file,
        bindings: [
          {:server_port, ["server", "port"]},
          {:store_database_file, ["store", "database_file"], map: &Path.expand/1}
        ]
      }
    ]

    Map.merge(config, Vapor.load!(providers))
  end

  @spec ensure_config_file!(String.t()) :: :ok
  defp ensure_config_file!(config_file) do
    if !File.exists?(config_file) do
      File.copy!("#{:code.priv_dir(@app)}/config.toml", config_file)
    end

    :ok
  end

  @spec get_config_file() :: String.t()
  defp get_config_file() do
    if get_environment() == :dev do
      Path.join([System.tmp_dir(), "silver-brain.toml"])
    else
      case System.get_env(@env_var_config_file) do
        nil -> Path.expand("~/.silver-brain/config.toml")
        file -> file
      end
    end
  end

  defp get_environment() do
    Application.get_env(@app, :environment)
  end
end
