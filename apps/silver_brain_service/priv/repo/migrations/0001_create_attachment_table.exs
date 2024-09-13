defmodule SilverBrain.Service.Repo.Migrations.CreateAttachmentTable do
  use Ecto.Migration

  def up() do
    create table("attachments") do
      add(:id, :string, primary_key: true)
      add(:name, :string)

      timestamps()
    end
  end

  def down() do
    drop_if_exists("attachments")
  end
end
