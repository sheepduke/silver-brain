defmodule SilverBrain.Service.SqlItemStore do
  use TypedStruct
  alias SilverBrain.Core.Item
  alias SilverBrain.Service.RepoManager

  import Ecto.Query, only: [from: 2]

  typedstruct do
    field :repo_name, String.t(), enforce: true
  end
end

defimpl SilverBrain.Core.ItemStore, for: SilverBrain.Service.SqlItemStore do
  alias SilverBrain.Core.ItemStore
  alias SilverBrain.Core.Item
  alias SilverBrain.Service.SqlItemStore
  alias SilverBrain.Service.Repo
  alias SilverBrain.Service.RepoManager
  alias SilverBrain.Service.Schema

  import Ecto.Query, only: [from: 2]

  require Item

  # ============================================================
  #  Item
  # ============================================================

  def create_item(store = %SqlItemStore{}, name) when is_binary(name) do
    time = now_time()

    new_item = %Schema.Item{
      id: "i_" <> Ksuid.generate(),
      name: name,
      content_type: "",
      content: "",
      create_time: time,
      update_time: time
    }

    with :ok <- RepoManager.connect(store.repo_name),
         {:ok, _} <- Repo.insert(new_item) do
      {:ok, new_item.id}
    end
  end

  def get_item(store = %SqlItemStore{}, item_id) when is_binary(item_id) do
    with :ok <- RepoManager.connect(store.repo_name) do
      internal_get_item(item_id, [:id, :name, :content_type, :content, :create_time, :update_time])
    end
  end

  def get_item(store = %SqlItemStore{}, item_id, select)
      when is_binary(item_id) and is_list(select) do
    allowed_fields = [
      :id,
      :name,
      :content_type,
      :content,
      :create_time,
      :update_time,
      :properties
    ]

    field_allowed? = fn field ->
      Enum.member?(allowed_fields, field)
    end

    if Enum.all?(select, field_allowed?) do
      with :ok <- RepoManager.connect(store.repo_name) do
        internal_get_item(item_id, select)
      end
    else
      {:error, :bad_request}
    end
  end

  def get_items(store = %SqlItemStore{}, item_ids, select)
      when is_list(item_ids) and is_list(select) do
  end

  def update_item(store = %SqlItemStore{}, item = %ItemStore.UpdateItem{}) do
    with :ok <- RepoManager.connect(store.repo_name) do
      old_item = Repo.get_by(Schema.Item, id: item.id)
      changeset = Ecto.Changeset.change(old_item, Map.from_struct(item))

      with {:ok, _} <- Repo.update(changeset) do
        :ok
      end
    end
  end

  def delete_item(%SqlItemStore{repo_name: repo_name}, item_id) do
    with :ok <- RepoManager.connect(repo_name) do
      Repo.delete_all(from(item in Schema.Item, where: item.id == ^item_id))
      :ok
    end
  end

  # ============================================================
  #  Property
  # ============================================================

  def create_item_property(store, item_id, key, value) do
    property = %Schema.ItemProperty{
      item_id: item_id,
      key: key,
      value: value
    }

    Repo.insert(Schema.ItemProperty, property)
  end

  def delete_item_property(store, item_id, key) do
    from(property in Schema.ItemProperty,
      where: property.item_id == ^item_id and property.key == ^key
    )
    |> Repo.delete_all()
  end

  # ============================================================
  #  Link
  # ============================================================

  def create_child(store, parent_id, child_id) do
    query =
      from(link in Schema.ItemLink, where: link.parent == ^parent_id and link.child == ^child_id)

    now = now_time()

    case Repo.one(query) do
      nil ->
        Repo.insert(%Schema.ItemLink{
          parent: parent_id,
          child: child_id,
          create_time: now,
          update_time: now
        })

        :ok

      _ ->
        nil
    end
  end

  def get_parents(store, item_id) do
  end

  def get_children(store, item_id) do
  end

  def delete_child(store, item_id, child_id) do
  end

  # ============================================================
  #  Internal
  # ============================================================

  defp internal_get_item(item_id, select) when is_binary(item_id) and is_list(select) do
    # properties_selected? = Enum.member?(select, :properties)
    select = Enum.filter(select, fn x -> x != :properties end)

    case Repo.one(from(item in Schema.Item, where: item.id == ^item_id, select: ^select)) do
      nil ->
        {:error, :not_found}

      item ->
        {:ok,
         %Item{
           id: item.id,
           name: item.name,
           content_type: item.content_type,
           content: item.content,
           create_time: item.create_time,
           update_time: item.update_time
         }}
    end
  end

  defp now_time() do
    DateTime.utc_now() |> DateTime.truncate(:second)
  end
end

defimpl SilverBrain.Core.SearchEngine, for: SilverBrain.Service.SqlItemStore do
  alias SilverBrain.Service.Schema
  alias SilverBrain.Service.SqlSearchEngine
  alias SilverBrain.Service.Repo
  alias SilverBrain.Service.RepoManager
  alias SilverBrain.Core.SearchQuery
  alias SilverBrain.Service.SqlItemStore

  def search(store = %SqlItemStore{}, search_string) when is_binary(search_string) do
    with {:ok, query} <- SearchQuery.parse(search_string),
         RepoManager.connect(store.repo_name) do
      Repo.all(SqlSearchEngine.search(query))
      |> Enum.map(fn %Schema.Item{id: id} -> id end)
    end
  end
end
