defmodule SilverBrain.Repo.Migrations.CreateNewTables do
  use Ecto.Migration

  @doc """
  Create new tables with another name (concept_new, concept_link).
  """
  def change do
    create table(:concept_new) do
      add :uuid, :string, primary_key: true
      add :name, :string
      add :content_type, :string
      add :content, :string

      timestamps()
    end

    create index(:concept_new, [:name])

    create table(:concept_link) do
      add :source, :string, primary_key: true
      add :link, :string, primary_key: true
      add :target, :string, primary_key: true

      timestamps()
    end

    create index(:concept_link, [:source, :link, :target])
  end
end
