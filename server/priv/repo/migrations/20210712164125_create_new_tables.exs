defmodule SilverBrain.Repo.Migrations.CreateNewTables do
  use Ecto.Migration

  @doc """
  Create new tables with another name (concept, concept_link).
  """
  def change do
    # Enable foreign key support.
    execute "PRAGMA foreign_key = ON"

    create_if_not_exists table(:concept) do
      add :uuid, :string, primary_key: true
      add :name, :string
      add :content_type, :string
      add :content, :string

      timestamps()
    end

    create_if_not_exists index(:concept, [:name])

    create_if_not_exists table(:concept_link) do
      add :source, references(:concept, column: :uuid, type: :string),
        primary_key: true
      add :link, references(:concept, column: :uuid, type: :string),
        primary_key: true
      add :target, references(:concept, column: :uuid, type: :string),
        primary_key: true

      timestamps()
    end

    create_if_not_exists index(:concept_link, [:source, :link, :target])
  end
end
