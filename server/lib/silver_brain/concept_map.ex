defmodule SilverBrain.ConceptMap do
  alias SilverBrain.ConceptMap.{Concept}

  @spec get_concepts_by_uuid(String.t()) ::
          {:ok, Concept.t()} | {:error, :not_found} | {:error, :bad_request}
  def get_concepts_by_uuid(uuid) do
    {:ok, %Concept{uuid: uuid}}
  end
end
