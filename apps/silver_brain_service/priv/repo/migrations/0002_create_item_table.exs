defmodule SilverBrain.Service.Repo.Migrations.CreateItemTable do
  use Ecto.Migration

  def up() do
    create table("items") do
      add(:id, :string, primary_key: true)
      add(:name, :string)
      add(:content_type, :string)
      add(:content, :string)

      timestamps()
    end
  end
end
