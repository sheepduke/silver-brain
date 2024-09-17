defmodule SilverBrain.Service.RepoManager do
  alias SilverBrain.Service.RepoManager
  @type result :: SilverBrain.Core.result()
  @type result(t) :: SilverBrain.Core.result(t)

  # State of the server.
  defmodule State do
    use TypedStruct

    typedstruct do
      field :data_root_path, Path.t()
      field :repos, %{String.t() => pid()}
    end
  end

  require Logger
  use GenServer

  # ============================================================
  #  Client
  # ============================================================

  @spec start_link(Path.t()) :: any()
  def start_link(data_root_path) do
    state = %State{data_root_path: data_root_path, repos: %{}}

    with {:ok, pid} <- GenServer.start_link(__MODULE__, state, name: RepoManager),
         {:ok, repo_names} <- get_all() do
      for repo_name <- repo_names do
        migrate(repo_name)
      end

      {:ok, pid}
    end
  end

  @spec get_all() :: result(list(String.t()))
  def get_all() do
    GenServer.call(RepoManager, :get_all)
  end

  @spec connect(String.t()) :: result()
  def connect(repo_name) do
    with {:ok, pid} <- GenServer.call(RepoManager, {:connect, repo_name}),
         _ <- SilverBrain.Service.Repo.put_dynamic_repo(pid) do
      :ok
    end
  end

  @spec create(String.t()) :: result(list(String.t()))
  def create(repo_name) do
    with {:ok, _} <- GenServer.call(RepoManager, {:create, repo_name}) do
      migrate(repo_name)
    end
  end

  @spec migrate(String.t()) :: result(list(String.t()))
  def migrate(repo_name) do
    with :ok <- connect(repo_name) do
      Ecto.Migrator.run(SilverBrain.Service.Repo, :up, all: true)
    end
  end

  # ============================================================
  #  Server
  # ============================================================

  @impl true
  def init(state) do
    Logger.info("initializing with data_root_path=`#{state.data_root_path}`")

    case File.dir?(state.data_root_path) do
      true -> {:ok, state}
      false -> {:error, "data_root_path not a directory"}
    end
  end

  @impl true
  def handle_call(:get_data_root_path, _from, state) do
    {:reply, state.data_root_path, state}
  end

  @impl true
  def handle_call(:get_all, _from, state) do
    case File.ls(state.data_root_path) do
      {:ok, dirs} ->
        repo_names =
          dirs
          |> Enum.filter(&is_valid_repo(state, &1))

        {:reply, {:ok, repo_names}, state}

      _ ->
        {:error, "I/O error"}
    end
  end

  @impl true
  def handle_call({:connect, repo_name}, _from, state) do
    case Map.get(state.repos, repo_name) do
      nil ->
        if is_valid_repo(state, repo_name) do
          {:ok, pid} = start_repo(state, repo_name)
          repos = Map.put(state.repos, repo_name, pid)
          {:reply, {:ok, pid}, %State{state | :repos => repos}}
        else
          {:reply, {:error, "Requested repository does not exist"}, state}
        end

      pid ->
        {:reply, {:ok, pid}, state}
    end
  end

  @impl true
  def handle_call({:create, repo_name}, _from, state) do
    repo_path = Path.join(state.data_root_path, repo_name)

    if File.exists?(repo_path) do
      {:reply, {:error, "Target directory already exists"}, state}
    else
      case File.mkdir(repo_path) do
        :ok ->
          File.mkdir(repo_path)
          File.touch(Path.join(repo_path, "data.sqlite"))
          {:reply, {:ok, repo_name}, state}

        error ->
          {:reply, error, state}
      end
    end
  end

  defp is_valid_repo(state, repo_name) do
    File.regular?(Path.join([state.data_root_path, repo_name, "data.sqlite"]))
  end

  defp start_repo(state, repo_name) do
    db_config = [
      name: nil,
      database: Path.join([state.data_root_path, repo_name, "data.sqlite"])
    ]

    SilverBrain.Service.Repo.start_link(db_config)
  end
end
