defmodule SilverBrain.Service.RepoManager do
  alias SilverBrain.Service.RepoManager
  # alias SilverBrain.Service.RepoManager
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
    GenServer.start_link(
      __MODULE__,
      %State{data_root_path: data_root_path, repos: %{}},
      name: RepoManager
    )
  end

  def get_all() do
    GenServer.call(RepoManager, :get_all)
  end

  def connect(repo_name) do
    case GenServer.call(RepoManager, {:connect, repo_name}) do
      {:ok, pid} -> SilverBrain.Service.Repo.put_dynamic_repo(pid)
      error -> error
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
        repo_names = dirs |> Enum.filter(&is_valid_repo(state.data_root_path, &1))
        {:reply, {:ok, repo_names}, state}

      _ ->
        {:error, "I/O error"}
    end
  end

  @impl true
  def handle_call({:connect, repo_name}, _from, state) do
    case Map.get(state.repos, repo_name) do
      nil ->
        if is_valid_repo(state.data_root_path, repo_name) do
          {:ok, pid} = start_repo(state.data_root_path, repo_name)
          repos = Map.put(state.repos, repo_name, pid)
          {:reply, {:ok, pid}, %State{state | :repos => repos}}
        else
          {:reply, {:error, "Requested repository does not exist"}, state}
        end

      pid ->
        {:reply, {:ok, pid}, state}
    end
  end

  defp is_valid_repo(data_root_path, repo_name) do
    File.regular?(Path.join([data_root_path, repo_name, "data.sqlite"]))
  end

  defp start_repo(data_root_path, repo_name) do
    db_config = [
      name: nil,
      database: Path.join([data_root_path, repo_name, "data.sqlite"])
    ]

    SilverBrain.Service.Repo.start_link(db_config)
  end
end
