defmodule SilverBrain.ConceptMap do
  alias SilverBrain.ConceptMap.{Concept, Store}

  @spec get_concept_by_uuid(String.t()) :: {:ok, Concept.t()} | {:error, :not_found}
  def get_concept_by_uuid(uuid) do
    case Store.get_concept_by_uuid(uuid) do
      nil -> {:error, :not_found}
      concept -> {:ok, concept}
    end
  end
end
