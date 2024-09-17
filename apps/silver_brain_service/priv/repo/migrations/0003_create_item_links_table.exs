defmodule SilverBrain.Service.Repo.Migrations.CreateItemLinksTable do
  use Ecto.Migration

  @table_name "item_links"

  def up() do
    create table(@table_name) do
      add(:parent, :string)
      add(:child, :string)

      timestamps()
    end
  end

  def down() do
    drop_if_exists(@table_name)
  end
end
