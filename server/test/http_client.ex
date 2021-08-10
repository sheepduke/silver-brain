defmodule SilverBrainTest.HttpClient do
  use Tesla

  plug(Tesla.Middleware.BaseUrl, "http://localhost:4001/api")

  def get(url, query \\ []) do
    Tesla.get(url, query: query)
  end
end
