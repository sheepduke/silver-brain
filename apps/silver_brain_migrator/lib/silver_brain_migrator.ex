defmodule SilverBrain.Migrator do
  alias Ecto.Changeset
  alias SilverBrain.Service.RepoManager
  alias SilverBrain.Service.Repo
  alias SilverBrain.Service.Schema
  alias SilverBrain.Core.Item
  import Ecto.Query, only: [from: 2]

  def load_items() do
    RepoManager.connect("old")

    Repo.all(from("item", select: [:id, :props]))
    |> Enum.map(fn item ->
      {:ok, props} = Jason.decode(item.props)

      %{
        id: "i_" <> Map.get(props, "id"),
        name: Map.get(props, "name"),
        content_type: Map.get(props, "contentType"),
        content: Map.get(props, "content"),
        create_time: convert_datetime(Map.get(props, "createTime")),
        update_time: convert_datetime(Map.get(props, "updateTime"))
      }
    end)
  end

  def load_item_links() do
    RepoManager.connect("old")

    Repo.all(from("item_child", select: [:parent, :child, :create_time]))
    |> Stream.filter(&(Map.get(&1, :child) != "nil"))
    |> Enum.map(fn row ->
      datetime =
        row
        |> Map.get(:create_time)
        |> convert_datetime()

      row
      |> Map.update(:parent, nil, &convert_item_id/1)
      |> Map.update(:child, nil, &convert_item_id/1)
      |> Map.put(:create_time, datetime)
      |> Map.put(:update_time, datetime)
    end)
  end

  def load_item_references() do
    RepoManager.connect("old")

    Repo.all(
      from("item_reference",
        select: [:id, :source, :target, :annotation, :create_time, :update_time]
      )
    )
    |> Enum.map(fn row ->
      row
      |> Map.update(:id, nil, &convert_reference_id/1)
      |> Map.update(:source, nil, &convert_item_id/1)
      |> Map.update(:target, nil, &convert_item_id/1)
      |> Map.update(:create_time, nil, &convert_datetime/1)
      |> Map.update(:update_time, nil, &convert_datetime/1)
    end)
  end

  def convert_datetime(value) do
    {:ok, result, _} =
      value
      |> String.replace(" ", "T")
      |> DateTime.from_iso8601()

    result |> DateTime.truncate(:second)
  end

  def convert_reference_id(id), do: "r_" <> id

  def convert_item_id(id), do: "i_" <> id

  def refresh() do
    File.rm_rf!(Path.expand("~/temp/test/new"))
    File.rm_rf!(Path.expand("~/temp/test/old"))

    File.cp_r!(Path.expand("~/.silver-brain/main"), Path.expand("~/temp/test/old"))
    RepoManager.create("new")
  end

  def run() do
    items = load_items()
    item_links = load_item_links()
    item_references = load_item_references()

    RepoManager.connect("new")

    Repo.insert_all(Schema.Item, items)
    Repo.insert_all(Schema.ItemLink, item_links)
    Repo.insert_all(Schema.ItemReference, item_references)

    count_all()
  end

  def count_all() do
    {{:item, Repo.one(from(Schema.Item, select: count()))},
     {:item_link, Repo.one(from(Schema.ItemLink, select: count()))},
     {:item_reference, Repo.one(from(Schema.ItemReference, select: count()))}}
  end
end

# SilverBrain.Migrator.refresh()
# SilverBrain.Migrator.run()
