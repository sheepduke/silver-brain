defmodule SilverBrain.Service.Repo.Migrations.CreateItemReferencesTable do
  use Ecto.Migration

  def change() do
    create table(:item_references) do
      add :id, :string, primary_key: true
      add :source, references(:items, column: :id, type: :string)
      add :target, references(:items, column: :id, type: :string)
      add :annotation, :string

      timestamps()
    end
  end
end
