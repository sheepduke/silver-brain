defmodule SilverBrain.Repo.Migrations.PurgeLegacyData do
  use Ecto.Migration

  @doc """
  Purge data in legacy table.
  """
  def change do
    drop_if_exists table("concept")
    drop_if_exists table("concept_relation")
    rename table("concept_new"), to: table("concept")
  end
end
