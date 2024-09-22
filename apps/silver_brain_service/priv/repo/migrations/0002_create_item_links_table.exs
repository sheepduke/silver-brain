defmodule SilverBrain.Service.Repo.Migrations.CreateItemLinksTable do
  use Ecto.Migration

  def change() do
    create table(:item_links) do
      add :parent, references(:items, column: :id, type: :string), primary_key: true
      add :child, references(:items, column: :id, type: :string), primary_key: true

      timestamps()
    end
  end
end
