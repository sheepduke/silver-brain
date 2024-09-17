defmodule SilverBrain.Service.Repo.Migrations.CreateItemReferencesTable do
  use Ecto.Migration

  @table_name "item_references"

  def up() do
    create table(@table_name) do
      add(:id, :string, primary_key: true)
      add(:source, :string)
      add(:target, :string)
      add(:annotation, :string)

      timestamps()
    end
  end

  def down() do
    drop_if_exists(@table_name)
  end
end
