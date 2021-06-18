defmodule SilverBrain.Repo.Migrations.CreateConceptAndRelation do
  use Ecto.Migration

  def change do
    create_if_not_exists table("concept") do
      add :uuid, :string, primary_key: true
    end
  end

end
