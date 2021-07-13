defmodule SilverBrain.Repo.Migrations.MigrateLegacyData do
  use Ecto.Migration
  import Ecto.Query, only: [from: 2]

  @doc """
  Migrate data from old table (concept) to new one (concept_new).
  """
  def up() do
    # Insert existing concepts to new table.
    query = from concept in "concept",
      select: %{
        uuid: concept.uuid,
        name: concept.name,
        content_type: ^"text/org", # concept.content_format,
        content: concept.content,
        inserted_at: concept.created_at,
        updated_at: concept.updated_at
      },
      where: true
    SilverBrain.Repo.insert_all("concept_new", query)

    # Insert a new concept "Contains".
    contains_uuid = Ecto.UUID.generate()
    now_time = DateTime.utc_now()

    SilverBrain.Repo.insert_all("concept_new", [[
      uuid: contains_uuid,
      name: "Contains",
      inserted_at: now_time,
      updated_at: now_time
    ]])

    # Populate concept_link table with newly created "Contains" concept.
    query = from relation in "concept_relation",
      select: %{
        source: relation.source,
        link: ^contains_uuid,
        target: relation.target,
        inserted_at: relation.created_at,
        updated_at: relation.updated_at
      },
      where: true
    SilverBrain.Repo.insert_all("concept_link", query)
  end

  @doc """
  Delete inserted data in concept_new table.
  """
  def down() do
    SilverBrain.Repo.delete_all("concept_new")
    SilverBrain.Repo.delete_all("concept_link")
  end
end
