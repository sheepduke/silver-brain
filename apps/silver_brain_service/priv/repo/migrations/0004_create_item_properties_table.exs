defmodule SilverBrain.Service.Repo.Migrations.CreateItemPropertiesTable do
  use Ecto.Migration

  def change() do
    create table(:item_properties) do
      add :id, :string, primary_key: true
      add :item_id, references(:items)
      add :key, :string
      add :value, :string

      timestamps()
    end
  end
end
