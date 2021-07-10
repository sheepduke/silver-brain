defmodule SilverBrain.Config.App do
  @type t :: %__MODULE__{
          home_dir: String.t(),
          work_dir: String.t()
        }

  defstruct [:home_dir, :work_dir]
end

defmodule SilverBrain.Config.Store do
  @type t :: %__MODULE__{
          root_dir: String.t(),
          database_file: String.t()
        }

  defstruct [:root_dir, :database_file]
end

defmodule SilverBrain.Config do
  @app :silver_brain
  @env_key :config
  @env_var_config_file "APP_CON FIG_FILE"

  @type t :: %__MODULE__{
          app: SilverBrain.Config.App.t()
        }

  defstruct [:app, :store]

  @spec get() :: SilverBrain.Config.t()
  def get() do
    case Application.get_env(@app, @env_key, nil) do
      nil ->
        config = load!()
        Application.put_env(@app, @env_key, config)
        config

      config ->
        config
    end
  end

  @spec load!() :: SilverBrain.Config.t()
  defp load!() do
    {:ok, home_dir} = System.fetch_env("HOME")
    {:ok, work_dir} = File.cwd()

    config_file = get_config_file(work_dir, home_dir)

    providers = [
      %Vapor.Provider.File{
        path: config_file,
        bindings: [
          store_root_dir: ["store", "root_dir"],
          store_database_file: ["store", "database_file"]
        ]
      }
    ]

    config = Vapor.load!(providers)

    %__MODULE__{
      app: %SilverBrain.Config.App{
        home_dir: home_dir,
        work_dir: work_dir
      },
      store: %SilverBrain.Config.Store{
        root_dir: Path.expand(config.store_root_dir),
        database_file: config.store_database_file
      }
    }
  end

  @spec get_config_file(String.t(), String.t()) :: String.t()
  defp get_config_file(work_dir, home_dir) do
    if Mix.env() == :dev do
      Path.join([work_dir, "config.toml"])
    else
      case System.get_env(@env_var_config_file) do
        nil -> Path.join([home_dir, ".silver-brain", "config.toml"])
        file -> file
      end
    end
  end
end
