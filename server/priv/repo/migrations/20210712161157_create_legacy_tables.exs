defmodule SilverBrain.Repo.Migrations.CreateLegacyTables do
  use Ecto.Migration

  @doc """
  Create legacy tables (concept and concept_relation).
  """
  def change do
    create_if_not_exists table(:concept) do
      add :uuid, :varchar, size: 64, null: false
      add :name, :varchar, size: 1024, null: false
      add :content, :varchar, size: 1024, null: false
      add :content_format, :varchar, size: 16, null: false
      add :created_at, :utc_datetime
      add :updated_at, :utc_datetime
    end

    create_if_not_exists table(:concept_relation, primary_key: true) do
      add :source, :varchar, size: 64, null: false
      add :target, :varchar, size: 64, null: false
      add :created_at, :utc_datetime
      add :updated_at, :utc_datetime
    end
  end
end
