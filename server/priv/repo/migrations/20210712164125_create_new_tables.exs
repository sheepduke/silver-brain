defmodule SilverBrain.Repo.Migrations.CreateNewTables do
  use Ecto.Migration

  @doc """
  Create new tables with another name (concept_new, concept_link).
  """
  def change do
    # Enable foreign key support.
    execute "PRAGMA foreign_key = ON"

    create_if_not_exists table(:concept_new) do
      add :uuid, :string, primary_key: true
      add :name, :string
      add :content_type, :string
      add :content, :string

      timestamps()
    end

    create_if_not_exists index(:concept_new, [:name])

    create_if_not_exists table(:concept_link) do
      add :source, references("concept_new", column: :uuid, type: :string),
        primary_key: true
      add :link, references("concept_new", column: :uuid, type: :string),
        primary_key: true
      add :target, references("concept_new", column: :uuid, type: :string),
        primary_key: true

      timestamps()
    end

    create_if_not_exists index(:concept_link, [:source, :link, :target])
  end
end
