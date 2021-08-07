defmodule SilverBrain.Repo.Concept do
  use Ecto.Schema

  @primary_key {:uuid, :string, []}
  schema "concept" do
    field(:name, :string)
    field(:content_type, :string)
    field(:content, :string)
    timestamps()
  end
end
