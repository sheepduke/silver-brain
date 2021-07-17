defmodule SilverBrain.Repo.Migrations.PurgeLegacyData do
  use Ecto.Migration

  @doc """
  Purge data in legacy table.
  """
  def change do
    drop_if_exists table("concept_legacy")
    drop_if_exists table("concept_relation")
  end
end
