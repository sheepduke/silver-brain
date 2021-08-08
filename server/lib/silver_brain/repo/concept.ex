defmodule SilverBrain.Repo.Concept do
  use SilverBrain.Repo.Schema

  @primary_key {:uuid, :string, []}
  schema "concept" do
    field(:name, :string)
    field(:content_type, :string)
    field(:content, :string)
    timestamps()
  end
end
