defmodule SilverBrain.ConceptMap.Store do
  alias SilverBrain.ConceptMap.{Concept}
  alias SilverBrain.Repo

  @spec get_concept_by_uuid(String.t()) :: Concept.t() | nil
  def get_concept_by_uuid(uuid) do
    case Repo.get(Repo.Concept, uuid) do
      nil -> nil
      repo_concept -> struct(Concept, Map.from_struct(repo_concept))
    end
  end
end
