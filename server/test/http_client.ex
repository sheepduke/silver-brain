defmodule SilverBrainTest.HttpClient do
  use Tesla

  plug(Tesla.Middleware.BaseUrl, "http://localhost:4001/api")

  def get(url) do
    Tesla.get(url)
  end
end
