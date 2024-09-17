defmodule SilverBrain.Service.Repo.Migrations.CreateItemsTable do
  use Ecto.Migration

  @table_name "items"

  def up() do
    create table(@table_name) do
      add(:id, :string, primary_key: true)
      add(:name, :string)
      add(:content_type, :string)
      add(:content, :string)

      timestamps()
    end
  end

  def down() do
    drop_if_exists(@table_name)
  end
end
