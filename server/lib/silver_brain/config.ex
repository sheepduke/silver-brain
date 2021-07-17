defmodule SilverBrain.Config.App do
  @app :silver_brain

  @type t :: %__MODULE__{
          migration_dir: String.t()
        }

  defstruct [:migration_dir]

  @doc """
  Load application level configurations.
  """
  @spec load!() :: SilverBrain.Config.App.t()
  def load!() do
    migration_dir = Application.app_dir(@app, "priv/repo/migrations")

    %SilverBrain.Config.App{
      migration_dir: migration_dir
    }
  end
end

defmodule SilverBrain.Config.Store do
  @type t :: %__MODULE__{
          database_file: String.t()
        }

  defstruct [:database_file]
end

defmodule SilverBrain.Config do
  alias SilverBrain.Config

  @app :silver_brain
  @env_key :config
  @env_var_config_file "APP_CON FIG_FILE"

  @type t :: %__MODULE__{
          app: SilverBrain.Config.App.t(),
          store: SilverBrain.Config.Store.t()
        }

  defstruct [:app, :store]

  @doc """
  Get configuration struct.
  """
  @spec get() :: SilverBrain.Config.t()
  def get() do
    case Application.get_env(@app, @env_key, nil) do
      nil -> init!()
      config -> config
    end
  end

  @doc """
  Load configuration and set it to application environment.
  """
  @spec init!() :: SilverBrain.Config.t()
  def init!() do
    app_config = Config.App.load!()

    config_file = get_config_file()
    ensure_config_file!(config_file)

    providers = [
      %Vapor.Provider.File{
        path: config_file,
        bindings: [
          store_database_file: ["store", "database_file"]
        ]
      }
    ]

    custom_config = Vapor.load!(providers)
    database_file = Path.expand(custom_config.store_database_file)

    config = %__MODULE__{
      app: app_config,
      store: %SilverBrain.Config.Store{
        database_file: database_file
      }
    }

    Application.put_env(@app, @env_key, config)
    config
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
    if Mix.env() == :dev do
      "/tmp/silver-brain.toml"
    else
      case System.get_env(@env_var_config_file) do
        nil -> Path.expand("~/.silver-brain/config.toml")
        file -> file
      end
    end
  end
end
