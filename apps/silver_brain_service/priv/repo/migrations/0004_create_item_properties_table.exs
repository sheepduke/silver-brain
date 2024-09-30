defmodule SilverBrain.Service.Repo.Migrations.CreateItemPropertiesTable do
  use Ecto.Migration

  def change() do
    create table(:item_properties) do
      add :item_id, references(:items, column: :id, type: :string), primary_key: true
      add :key, :string, primary_key: true
      add :value, :string

      timestamps()
    end
  end
end
