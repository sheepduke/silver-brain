defmodule SilverBrain.Repo.Schema do
  defmacro __using__(_opts) do
    quote do
      use Ecto.Schema

      @timestamps_opts [
        type: :utc_datetime,
        inserted_at: :create_time,
        updated_at: :update_time
      ]
    end
  end
end
