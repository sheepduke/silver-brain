defmodule SilverBrain.Config.App do
  @type t :: %__MODULE__{
          home_dir: String.t(),
          work_dir: String.t()
        }

  defstruct [:home_dir, :work_dir]
end

defmodule SilverBrain.Config.Store do
  @type t :: %__MODULE__{
          database_file: String.t()
        }

  defstruct [:database_file]
end

defmodule SilverBrain.Config do
  @app :silver_brain
  @env_key :config
  @env_var_config_file "APP_CON FIG_FILE"

  @type t :: %__MODULE__{
          app: SilverBrain.Config.App.t(),
          store: SilverBrain.Config.Store.t()
        }

  defstruct [:app, :store]

  @spec get() :: SilverBrain.Config.t()
  def get() do
    case Application.get_env(@app, @env_key, nil) do
      nil -> init!()
      config -> config
    end
  end

  @spec init!() :: SilverBrain.Config.t()
  defp init!() do
    {:ok, home_dir} = System.fetch_env("HOME")
    {:ok, work_dir} = File.cwd()

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

    config = Vapor.load!(providers)

    database_file = Path.expand(config.store_database_file)

    %__MODULE__{
      app: %SilverBrain.Config.App{
        home_dir: home_dir,
        work_dir: work_dir
      },
      store: %SilverBrain.Config.Store{
        database_file: database_file
      }
    }
  end

  @spec ensure_config_file!(String.t()) :: :ok
  defp ensure_config_file!(config_file) do
    if !File.exists?(config_file) do
      File.copy!("#{:code.priv_dir(@app)}/config.toml", config_file)
    end
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
