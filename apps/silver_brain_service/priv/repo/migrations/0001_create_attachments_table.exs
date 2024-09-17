defmodule SilverBrain.Service.Repo.Migrations.CreateAttachmentsTable do
  use Ecto.Migration

  @table_name "attachments"

  def up() do
    create table(@table_name) do
      add(:id, :string, primary_key: true)
      add(:name, :string)

      timestamps()
    end
  end

  def down() do
    drop_if_exists(@table_name)
  end
end
