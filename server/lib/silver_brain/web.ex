defmodule SilverBrain.Web do
  alias SilverBrain.ConceptMap

  use Plug.Router

  plug(Plug.Logger)
  plug(:match)
  plug(:dispatch)

  get "/api/concept/:uuid" do
    conn = Plug.Conn.fetch_query_params(conn)
    params = conn.query_params |> IO.inspect()
    result = ConceptMap.get_concept_by_uuid(uuid, params)
    send_json_resp(conn, result)
  end

  defp send_json_resp(conn, term) do
    response =
      case term do
        {:ok, term} -> resp(conn, 200, Jason.encode!(term))
        {:error, :not_found} -> resp(conn, 404, "")
        {:error, :bad_request} -> resp(conn, 400, "")
      end

    send_resp(response)
  end
end
