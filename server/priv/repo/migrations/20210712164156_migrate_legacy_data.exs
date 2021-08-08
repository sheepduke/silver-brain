defmodule SilverBrain.Repo.Migrations.MigrateLegacyData do
  use Ecto.Migration
  import Ecto.Query, only: [from: 2]

  @doc """
  Migrate data from old table (concept) to new one (concept).
  """
  def up() do
    # Enable foreign key support.
    execute "PRAGMA foreign_key = ON"
    
    # Insert existing concepts to new table.
    query = from concept in "concept_legacy",
      select: %{
        uuid: concept.uuid,
        name: concept.name,
        content_type: ^"text/org", # concept.content_format,
        content: concept.content,
        create_time: concept.created_at,
        update_time: concept.updated_at
      },
      where: true
    SilverBrain.Repo.insert_all("concept", query)

    # Insert a new concept "Contains".
    contains_uuid = Ecto.UUID.generate()
    now_time = DateTime.utc_now()

    SilverBrain.Repo.insert_all("concept", [[
      uuid: contains_uuid,
      name: "Contains",
      create_time: now_time,
      update_time: now_time
    ]])

    # Populate concept_link table with newly created "Contains" concept.
    query = from relation in "concept_relation",
      select: %{
        source: relation.source,
        link: ^contains_uuid,
        target: relation.target,
        create_time: relation.created_at,
        update_time: relation.updated_at
      },
      where: true
    relations = SilverBrain.Repo.all(query)
    |> Enum.uniq_by(&(&1.source <> &1.target))

    SilverBrain.Repo.insert_all("concept_link", relations)
  end

  @doc """
  Delete inserted data in concept table.
  """
  def down() do
    SilverBrain.Repo.delete_all("concept")
    SilverBrain.Repo.delete_all("concept_link")
  end
end
