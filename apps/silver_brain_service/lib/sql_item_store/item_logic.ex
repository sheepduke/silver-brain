defmodule SilverBrain.Service.SqlItemStore.ItemLogic do
  alias SilverBrain.Core.ItemStore
  alias SilverBrain.Service.Schema.ItemProperty
  alias SilverBrain.Core.Item
  alias SilverBrain.Service.Repo
  alias SilverBrain.Service.RepoManager
  alias SilverBrain.Service.Schema
  alias SilverBrain.Service.SqlItemStore

  import Ecto.Query, only: [from: 2]

  def create_item(store, name) do
    now_time = now_time()

    new_item = %Schema.Item{
      id: "i_" <> Ksuid.generate(),
      name: name,
      content_type: "",
      content: "",
      properties: [],
      create_time: now_time,
      update_time: now_time
    }

    Repo.insert(new_item)

    {:ok, new_item.id}
  end

  def get_item(store = %SqlItemStore{}, item_id) do
    internal_get_item(store, item_id, [
      :id,
      :name,
      :content_type,
      :content,
      :create_time,
      :update_time
    ])
  end

  def get_item(store, item_id, select) do
    allowed_properties = [
      :id,
      :name,
      :content_type,
      :content,
      :create_time,
      :update_time,
      :properties
    ]

    allowed_property? = fn property ->
      Enum.member?(allowed_properties, property)
    end

    if Enum.all?(select, allowed_property?) do
      internal_get_item(store, item_id, select)
    else
      {:error, :bad_request, "Invalid property specified"}
    end
  end

  def update_item(item = %ItemStore.UpdateItem{}) do
    old_item = Repo.get_by(Schema.Item, id: item.id)
    changeset = Ecto.Changeset.change(old_item, Map.from_struct(item))

    with {:ok, _} <- Repo.update(changeset) do
      :ok
    end
  end

  defp internal_get_item(store, item_id, select) do
    properties_wanted? = if Enum.member?(select, :properties), do: true, else: false
    select = Enum.filter(select, fn x -> x != :properties end)

    with :ok <- RepoManager.connect(store.repo_name) do
      case Repo.get_by(Schema.Item, [id: item_id], select: select) do
        db_item ->
          convert_item(db_item, select)

        nil ->
          {:error, :not_found}
      end
    end
  end

  defp convert_item(item = %Schema.Item{}, select) do
    selected? = fn x -> Enum.member?(select, x) end

    id = if selected?.(:id), do: item.id, else: nil
    name = if selected?.(:name), do: item.name, else: nil
    content_type = if selected?.(:content_type), do: item.content_type, else: nil
    content = if selected?.(:content), do: item.content, else: nil
    create_time = if selected?.(:create_time), do: item.create_time, else: nil
    update_time = if selected?.(:update_time), do: item.update_time, else: nil

    %Item{
      id: id,
      name: name,
      content_type: content_type,
      content: content,
      create_time: create_time,
      update_time: update_time
    }
  end

  defp now_time() do
    DateTime.utc_now() |> DateTime.truncate(:second)
  end
end
